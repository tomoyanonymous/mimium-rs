use crate::plot_ui::{self, PlotUi};
use eframe;

use egui::Color32;
use egui_plot::{CoordinatesFormatter, Corner, Legend, Plot};
use ringbuf::HeapCons;

#[derive(Default)]
pub struct PlotApp {
    plot: Vec<plot_ui::PlotUi>,
    hue: f32,
    autoscale: bool,
}

impl PlotApp {
    pub fn new_test() -> Self {
        let plot = vec![PlotUi::new_test("test")];
        Self {
            plot,
            hue: 0.0,
            autoscale: false,
        }
    }
    const HUE_MARGIN: f32 = 1.0 / 8.0 + 0.3;
    pub fn add_plot(&mut self, label: &str, buf: HeapCons<f64>) {
        let [r, g, b] = egui::ecolor::Hsva::new(self.hue, 0.7, 0.7, 1.0).to_srgb();
        self.hue += Self::HUE_MARGIN;
        self.plot.push(PlotUi::new(
            label,
            buf,
            Color32::from_rgba_premultiplied(r, g, b, 200),
        ))
    }
}

impl eframe::App for PlotApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
            // The top panel is often a good place for a menu bar:

            egui::menu::bar(ui, |ui| {
                egui::widgets::global_theme_preference_buttons(ui);
                ui.add_space(16.0);
                use egui::special_emojis::GITHUB;
                ui.hyperlink_to(
                    format!("{GITHUB} mimium-rs on GitHub"),
                    "https://github.com/tomoyanonymous/mimium-rs",
                );
                ui.checkbox(&mut self.autoscale, "Auto Scale")
            });
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            let plot = Plot::new("lines_demo")
                .legend(Legend::default())
                .show_axes(true)
                .show_grid(true)
                .auto_bounds([true,self.autoscale].into())
                .coordinates_formatter(Corner::LeftBottom, CoordinatesFormatter::default());

            plot.show(ui, |plot_ui| {
                self.plot.iter_mut().for_each(|line| {
                    let (_req_repaint, line) = line.draw_line();
                    plot_ui.line(line);
                })
            });

            ui.ctx().request_repaint();
        });
    }
}
