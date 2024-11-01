use egui::Color32;
use egui_plot::{CoordinatesFormatter, Corner, Legend, Line, LineStyle, Plot, PlotPoints};
use ringbuf::{traits::Consumer, HeapCons, HeapRb};
pub(crate) struct PlotUi {
    label: String,
    show_axes: bool,
    show_grid: bool,
    buf: HeapCons<f64>,
    local_buf: Vec<f64>,
    update_index:usize,
}
impl PlotUi {
    pub fn new(label: &str, buf: HeapCons<f64>) -> Self {
        let local_buf = (0..512)
            .into_iter()
            .map(|_| 0.0)
            .collect();
        Self {
            label:label.to_string(),
            show_axes: true,
            show_grid: true,
            buf,
            local_buf,
            update_index:0
        }
    }
    pub fn new_test(label: &str) -> Self {
        let local_buf = (0..512)
            .into_iter()
            .map(|t| (6.28 * (t as f64 / 512.0)).sin())
            .collect();
        Self {
            label:label.to_string(),
            show_axes: true,
            show_grid: true,
            buf: HeapCons::new(HeapRb::new(10).into()),
            local_buf,
            update_index:0
        }
    }
    fn draw_line(&mut self) -> Line {
        while let Some(s) = self.buf.try_pop(){
            self.local_buf[self.update_index] = s;
            self.update_index= (self.update_index+1)%self.local_buf.len();
        }

        Line::new(PlotPoints::from_parametric_callback(
            |t| (t, self.local_buf[t as usize]),
            0.0..512.0,
            512,
        ))
        .color(Color32::from_rgb(200, 100, 100))
        .style(LineStyle::Solid)
        .name(&self.label)
    }
}
impl egui::Widget for &mut PlotUi {
    fn ui(self, ui: &mut egui::Ui) -> egui::Response {
        let plot = Plot::new("lines_demo")
            .legend(Legend::default())
            .show_axes(self.show_axes)
            .show_grid(self.show_grid)
            .coordinates_formatter(Corner::LeftBottom, CoordinatesFormatter::default());
        plot.show(ui, |plot_ui| {
            plot_ui.line(self.draw_line());
        })
        .response
    }
}
