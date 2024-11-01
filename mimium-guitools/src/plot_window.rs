use crate::plot_ui::{self, PlotUi};
use eframe;
use egui::{Response, Widget};
use egui_plot::PlotResponse;
use mimium_lang::plugin::Plugin;
use ringbuf::{Cons, HeapCons};

pub struct PlotApp {
    plot: Vec<plot_ui::PlotUi>,
}

impl PlotApp {
    pub fn new_test() -> Self {
        let plot = vec![PlotUi::new_test("test")];
        Self { plot }
    }
    pub fn add_plot(&mut self, label: &str, buf: HeapCons<f64>) {
        self.plot.push(PlotUi::new(label, buf))
    }
}

impl eframe::App for PlotApp {
    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
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
            });
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            ui.horizontal_centered(|ui| {
                for plot in self.plot.iter_mut() {
                    ui.vertical_centered_justified(|ui| {
                        plot.ui(ui);
                    });
                }
            })
        });
    }
}
