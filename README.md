# 🖥️ Delphi_DesktopCapture
**Professional Desktop Duplication Components for Delphi with Network Streaming & Intelligent Dirty Pixel Detection**

<div align="center">

![Version](https://img.shields.io/badge/Version-1.0-blue?style=for-the-badge)
![Delphi](https://img.shields.io/badge/Delphi-XE2%2B-red?style=for-the-badge)
![Platform](https://img.shields.io/badge/Platform-Windows-green?style=for-the-badge)
![License](https://img.shields.io/badge/License-MIT-orange?style=for-the-badge)

*High-performance DXGI-based desktop capture components with intelligent dirty pixel detection, network streaming capabilities, and automatic fallback to GDI*

</div>

---

Preview Images:

<Center>![Network Preview](Preview1.png) </Center>

<Center> ![Network Preview](Preview2.png) </Center>

## 🚀 Overview

Delphi_DesktopCapture provides two powerful, professional-grade desktop capture components built on Windows Desktop Duplication API (DXGI) with intelligent dirty pixel detection, automatic format optimization, network-ready TBytes streaming, and seamless GDI fallback. Perfect for remote desktop applications, screen sharing, surveillance software, live streaming, and real-time desktop monitoring.

### 🎯 What's Included

- **🖥️ TDesktopCapture Component** - Professional desktop duplication with intelligent dirty region detection
- **📺 TDesktopCaptureReceiver Component** - High-performance frame reconstruction and display
- **🔄 Smart Frame Management** - Full frame first, then dirty pixels only for maximum efficiency
- **🌐 Network-Ready Design** - TBytes-based callbacks for seamless network integration
- **🎯 Cursor Integration** - Optional cursor capture with proper alpha blending
- **📦 Ready-to-Install Package** - Complete component package for Delphi IDE
- **⚡ DXGI + GDI Fallback** - Maximum compatibility across all Windows versions

---

## 🏗️ Architecture

```mermaid
graph TD
    A[Desktop] --> B[DXGI Desktop Duplication]
    B --> C{DXGI Available?}
    C -->|Yes| D[High-Performance Capture]
    C -->|No| E[GDI Fallback]
    D --> F[TDesktopCapture]
    E --> F
    F --> G{First Frame?}
    G -->|Yes| H[Send Full Frame]
    G -->|No| I[Dirty Detection]
    H --> J[Store Base Frame]
    I --> K{Changes Found?}
    K -->|Yes| L[Send Dirty Regions]
    K -->|No| M[Skip Frame]
    L --> N[TBytes Network Stream]
    H --> N
    N --> O[TDesktopCaptureReceiver]
    O --> P{Frame Type?}
    P -->|Full| Q[Replace Base Frame]
    P -->|Dirty| R[Apply Dirty Regions]
    Q --> S[Update Display]
    R --> S
    S --> T[Target Image/Bitmap]
```

---

## ⭐ Key Features

### 🖥️ **TDesktopCapture Component**
- **DXGI Desktop Duplication** - Native Windows desktop duplication API for zero-copy capture
- **Intelligent Dirty Detection** - Advanced block-based algorithm detects only changed screen regions
- **Full Frame + Dirty Mode** - Sends complete screen first, then only pixel changes thereafter
- **Automatic GDI Fallback** - Seamless fallback to GDI on older systems or DXGI failures
- **High-Performance Threading** - Non-blocking capture with configurable frame rates
- **Network-Optimized Output** - TBytes format perfect for network transmission
- **Cursor Capture** - Optional high-quality cursor integration with alpha blending
- **Resolution Change Detection** - Automatic handling of display resolution changes

### 📺 **TDesktopCaptureReceiver Component**
- **Smart Frame Reconstruction** - Efficiently rebuilds full desktop from dirty regions
- **Base Frame Management** - Maintains complete desktop state for dirty region application
- **Display Integration** - Direct TImage component integration for easy UI development
- **Thread-Safe Operation** - Proper synchronization for multi-threaded network applications
- **Memory Efficient** - Intelligent buffer management prevents memory leaks
- **Error Resilient** - Graceful handling of corrupted or incomplete frame data

### 🎯 **Intelligent Dirty Detection**
- **Block-Based Analysis** - 32x32 pixel blocks for optimal performance vs. accuracy
- **Pixel Sampling** - Strategic pixel sampling reduces CPU usage while maintaining accuracy
- **Change Threshold** - Smart detection prevents false positives from minor variations
- **Region Optimization** - Coalesces adjacent dirty blocks into larger regions
- **Skip Empty Frames** - Automatically skips transmission when no changes detected

### 🛠️ **Technical Excellence**
- **Dual Capture Methods** - DXGI for performance, GDI for compatibility
- **Format Flexibility** - 32-bit BGRA pixel format with proper stride handling
- **Memory Efficient** - Smart buffer allocation and proper cleanup procedures
- **Error Resilient** - Comprehensive error handling and graceful degradation
- **Performance Optimized** - Direct memory operations for maximum throughput

---

## 📦 Installation

### Prerequisites
- **Delphi XE2 or later**
- **Windows Vista+** (DXGI requires Windows 8+ for best performance)
- **NetCom7 components** (recommended) - For network streaming examples

### Installation Steps
1. Extract components to your Delphi components directory
2. Open `DesktopCaptureComponents.dpk` in Delphi IDE
3. Build and Install the package
4. Add "RequiredHeaders" Folder to your IDE's Library path 32bit and 64bit paths if you plan to make both 32bit and 64bit binaries!
5. Also do not forget to do the same 32bit library path and 64bit library paths for the main components themselves in the "DesktopCaptureComponents" folder. 
6. Components appear on **"Desktop Capture"** tab simply drag and drop to use them!

---

## 🚀 Usage Examples

### Basic Desktop Capture with Dirty Detection
```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  DesktopCapture1.Mode := cmDirtyOnly;
  DesktopCapture1.OnFrameCaptured := OnFrameCaptured;
  DesktopCapture1.Active := True;
end;

procedure TForm1.OnFrameCaptured(Sender: TObject; const FrameData: TBytes; IsFullFrame: Boolean);
begin
  // That's it - FrameData contains everything needed
  // First frame = full screen, subsequent = dirty regions only
end;
```

### Local Desktop Display with Receiver
```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  DesktopCapture1.OnFrameCaptured := OnLocalFrame;
  DesktopCapture1.Active := True;
  DesktopReceiver1.TargetImage := Image1;
end;

procedure TForm1.OnLocalFrame(Sender: TObject; const FrameData: TBytes; IsFullFrame: Boolean);
begin
  DesktopReceiver1.ReceiveFrameData(FrameData);
end;
```

### Network Desktop Streaming (Sender)
```pascal
// Sender Side - ONE LINE to send over NetCom7
procedure TForm1.OnFrameCaptured(Sender: TObject; const FrameData: TBytes; IsFullFrame: Boolean);
begin
  NetCom7Client.ExecCommand(1, FrameData);  // DONE!
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DesktopCapture1.OnFrameCaptured := OnFrameCaptured;
  DesktopCapture1.Active := True;
end;
```

### Network Desktop Streaming (Receiver)
```pascal
// Receiver Side - ONE LINE to receive and display
procedure TForm1.NetCom7Server1ExecCommand(Sender: TObject; Socket: TCustomWinSocket; 
  const Command: Integer; Data: TBytes);
begin
  if Command = 1 then
    DesktopReceiver1.ReceiveFrameData(Data);  // DONE!
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DesktopReceiver1.TargetImage := Image1;
  NetCom7Server1.Start;
end;
```

### Complete Remote Desktop Application
```pascal
// CLIENT (Sender) - Complete in 4 lines
procedure TForm1.FormCreate(Sender: TObject);
begin
  DesktopCapture1.OnFrameCaptured := procedure(Sender: TObject; const FrameData: TBytes; IsFullFrame: Boolean)
    begin NetCom7Client.ExecCommand(1, FrameData); end;
  DesktopCapture1.Active := True;
  NetCom7Client.Connect('192.168.1.100', 8080);
end;

// SERVER (Receiver) - Complete in 3 lines  
procedure TForm1.NetCom7Server1ExecCommand(Sender: TObject; Socket: TCustomWinSocket; 
  const Command: Integer; Data: TBytes);
begin
  if Command = 1 then DesktopReceiver1.ReceiveFrameData(Data);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DesktopReceiver1.TargetImage := Image1;
  NetCom7Server1.Start;
end;
```

### Different Use Cases - Simple Configuration
```pascal
// High FPS for gaming/recording
DesktopCapture1.TargetFPS := 60;
DesktopCapture1.Mode := cmFullFrame;

// Bandwidth efficient for remote desktop  
DesktopCapture1.TargetFPS := 20;
DesktopCapture1.Mode := cmDirtyOnly;

// Low bandwidth surveillance
DesktopCapture1.TargetFPS := 5;
DesktopCapture1.Mode := cmDirtyOnly;
DesktopCapture1.IncludeCursor := False;
```

---

## 🎛️ Component Properties

### TDesktopCapture Properties
| Property | Type | Default | Description |
|----------|------|---------|-------------|
| **Active** | Boolean | False | Start/stop desktop capture |
| **Method** | TCaptureMethod | cmDXGI | Capture method (DXGI or GDI) |
| **Mode** | TCaptureMode | cmDirtyOnly | Frame mode (Full frames or dirty regions) |
| **TargetFPS** | Integer | 30 | Target capture frame rate (1-120) |
| **IncludeCursor** | Boolean | True | Include mouse cursor in capture |

### TDesktopCapture Events
| Event | Description |
|-------|-------------|
| **OnFrameCaptured** | Fired when frame data is captured (TBytes format, IsFullFrame flag) |

### TDesktopCaptureReceiver Properties
| Property | Type | Default | Description |
|----------|------|---------|-------------|
| **Active** | Boolean | True | Enable frame processing |
| **TargetImage** | TImage | nil | Target TImage component for display |
| **CurrentWidth** | Integer | Read-only | Current frame width |
| **CurrentHeight** | Integer | Read-only | Current frame height |

### TDesktopCaptureReceiver Events
| Event | Description |
|-------|-------------|
| **OnFrameReceived** | Fired when frame is processed (Width, Height parameters) |

### TDesktopCaptureReceiver Methods
| Method | Description |
|--------|-------------|
| **ReceiveFrameData(FrameData: TBytes)** | Process received frame data |
| **ClearDisplay** | Clear current display and reset state |
| **HasValidFrame: Boolean** | Check if valid frame data exists |

---

## 🎯 How Smart Dirty Detection Works

### Initial Full Frame
```pascal
// First capture always sends complete desktop
OnFrameCaptured(Sender, FullScreenBytes, True);  // IsFullFrame = True
// Receiver stores this as base frame for dirty region application
```

### Subsequent Dirty Frames
```pascal
// Component compares current screen with previous frame
// Detects changed 32x32 pixel blocks
// Sends only the changed regions + coordinates
OnFrameCaptured(Sender, DirtyRegionBytes, False); // IsFullFrame = False
// Receiver applies dirty regions to base frame
```

### Bandwidth Efficiency Example
```
Full Frame: 1920x1080x4 = 8,294,400 bytes
Typical Dirty Frame: 50 changed blocks = ~200,000 bytes
Bandwidth Reduction: ~97% for typical desktop usage
```

---

## 🎯 Use Cases

### 🏢 **Business & Enterprise**
- **Remote Desktop Solutions** - Full-featured remote desktop with cursor support
- **Screen Sharing Applications** - Low-latency screen sharing for meetings
- **Digital Signage** - Efficient content distribution to multiple displays
- **Helpdesk Support** - Remote assistance with minimal bandwidth usage
- **Presentation Broadcasting** - Share presentations with minimal network impact

### 🎮 **Development & Gaming**
- **Game Streaming** - Efficient game screen capture and streaming
- **Development Tools** - Screen sharing for collaborative development
- **Testing Frameworks** - Automated UI testing with screen capture
- **Live Streaming Software** - Professional desktop capture for streaming
- **Remote Debugging** - Share debug sessions across team members

### 🎓 **Educational & Training**
- **Online Learning Platforms** - Teacher screen sharing with students
- **Training Software** - Capture and replay training sessions
- **Tutorial Creation** - High-quality screen recording for tutorials
- **Remote Classroom** - Interactive online classroom experiences
- **Software Demonstrations** - Efficient demo sharing and recording

### 🛡️ **Security & Monitoring**
- **Desktop Surveillance** - Monitor desktop activity with minimal storage
- **Session Recording** - Record user sessions for compliance
- **Security Monitoring** - Real-time desktop monitoring for security
- **Activity Logging** - Efficient desktop activity capture and storage
- **Forensic Analysis** - Capture desktop state for later analysis

---

## 🔧 Technical Implementation Details

### DXGI Desktop Duplication
```pascal
// The component uses Windows Desktop Duplication API
IDXGIOutputDuplication.AcquireNextFrame()  // Zero-copy desktop access
// Benefits:
// - Hardware accelerated
// - Minimal CPU usage  
// - Perfect pixel accuracy
// - Automatic dirty region hints from Windows
```

### Intelligent Dirty Detection Algorithm
```pascal
// 1. Divide screen into 32x32 pixel blocks
// 2. Sample every 2nd pixel in each block for performance
// 3. Compare with previous frame blocks
// 4. Mark changed blocks as dirty regions
// 5. Coalesce adjacent dirty blocks
// 6. Send only dirty region pixel data + coordinates
```

### Frame Data Format
```pascal
TFrameHeader = packed record
  IsFullFrame: Boolean;        // True = full screen, False = dirty regions
  Width: Integer;              // Frame width
  Height: Integer;             // Frame height  
  DataSize: Integer;           // Pixel data size in bytes
  DirtyRegionCount: Integer;   // Number of dirty regions (0 for full frame)
end;
// Followed by dirty region coordinates (if any)
// Followed by pixel data (BGRA format)
```

### Network Protocol Integration
```pascal
// Perfect for network streaming protocols:
// 1. Capture sends TBytes via OnFrameCaptured
// 2. Transmit TBytes over any network protocol
// 3. Receiver processes TBytes via ReceiveFrameData()
// 4. Automatic frame reconstruction and display
```

---

## 📈 Performance Specifications

### Capture Performance
- **DXGI Method:** <1ms capture time on modern hardware
- **GDI Method:** 5-15ms capture time (compatibility fallback)
- **Dirty Detection:** <2ms for 1920x1080 desktop
- **Memory Usage:** <50MB for typical operation
- **CPU Usage:** <5% on modern CPUs with DXGI

### Network Efficiency
- **Full Frame:** ~8MB for 1920x1080 (uncompressed)
- **Typical Dirty Frame:** 50KB-500KB depending on activity
- **Bandwidth Reduction:** 90-99% vs. continuous full frames
- **Frame Rates:** 1-120 FPS configurable
- **Latency:** <50ms end-to-end on local network

### Supported Configurations
- **Resolutions:** Any Windows-supported resolution
- **Multi-Monitor:** Primary display capture
- **Pixel Format:** 32-bit BGRA (4 bytes per pixel)
- **Windows Versions:** Vista+ (Windows 8+ recommended for DXGI)

---

## 🧪 Testing & Quality

### Automated Tests
- **Component Installation** - Package builds and installs correctly in all supported Delphi versions
- **DXGI Initialization** - Desktop duplication initializes properly on Windows 8+
- **GDI Fallback** - Automatic fallback works when DXGI unavailable
- **Dirty Detection Accuracy** - Changed regions detected correctly
- **Memory Management** - No memory leaks during extended operation
- **Network Integration** - TBytes format works correctly with network components
- **Resolution Changes** - Handles display resolution changes gracefully

### Manual Testing Checklist
- [ ] Components appear in IDE after installation
- [ ] Desktop capture produces valid frame data
- [ ] Dirty detection works accurately
- [ ] Full frame sent first, dirty regions thereafter
- [ ] Cursor capture functions properly
- [ ] Receiver reconstructs frames correctly
- [ ] Network streaming maintains quality
- [ ] Performance remains stable during extended use
- [ ] Error handling works for various failure scenarios

---

## 🛠️ Compatibility

### Delphi Versions
- **Delphi XE2** - Full compatibility
- **Delphi XE3-XE8** - Full compatibility  
- **Delphi 10.x Seattle+** - Full compatibility
- **Delphi 11.x Alexandria** - Full compatibility
- **Delphi 12.x Athens** - Full compatibility

### Windows Versions
- **Windows Vista/7** - GDI capture method (DXGI not available)
- **Windows 8/8.1** - Full DXGI support, recommended
- **Windows 10** - Full support with optimal performance
- **Windows 11** - Full support with latest features

---

## 🤝 Contributing

Contributions welcome!

---

## 📝 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for complete details.

---

## 👨‍💻 Author

**BitmasterXor**
- GitHub: [@BitmasterXor](https://github.com/BitmasterXor)
- Discord: BitmasterXor

---

## 🙏 Acknowledgments

- **Microsoft DXGI Team** - Desktop Duplication API for efficient screen capture
- **Delphi Community** - Continuous support and inspiration  
- **Desktop Duplication Developers** - Best practices and optimization techniques
- **Beta Testers** - Critical feedback during component development
- **NetCom7 Contributors** - Networking components that integrate perfectly
- **Performance Testers** - Feedback on dirty detection algorithms and efficiency

---

## 📚 Additional Resources

### Documentation
- [Windows Desktop Duplication API](https://docs.microsoft.com/en-us/windows/win32/direct3ddxgi/desktop-dup-api)
- [DXGI Programming Guide](https://docs.microsoft.com/en-us/windows/win32/direct3ddxgi/dx-graphics-dxgi)
- [Delphi Component Development Guide](https://docwiki.embarcadero.com/RADStudio/en/Creating_Components)

### Example Projects
- Basic local desktop display and capture
- Network desktop streaming (client/server)

---

<div align="center">

**⭐ Star this repository if these components help your desktop capture projects!**

**Made with ❤️ By BitmasterXor For the Delphi Community**

</div>
