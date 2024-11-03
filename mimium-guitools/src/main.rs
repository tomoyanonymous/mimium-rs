/// Standalone app for testing.
/// 
use mimium_guitools::plot_window::PlotApp;

// When compiling natively:
#[cfg(not(target_arch = "wasm32"))]
fn main() -> eframe::Result {
    // env_logger::init(); // Log to stderr (if you run with `RUST_LOG=debug`).

    let native_options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_inner_size([400.0, 300.0])
            .with_min_inner_size([300.0, 220.0])
            // .with_icon(
            //     // NOTE: Adding an icon is optional
            //     eframe::icon_data::from_png_bytes(&include_bytes!("../assets/icon-256.png")[..])
            //         .expect("Failed to load icon"),)
            ,
        ..Default::default()
    };
    eframe::run_native(
        "mimium guitools",
        native_options,
        Box::new(|_cc| Ok(Box::new(PlotApp::new_test()))),
    )
}