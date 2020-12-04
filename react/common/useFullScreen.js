import { useState, useEffect } from 'react';

function toFullScreen(dom = document.documentElement) {
  if (dom.requestFullscreen) {
    dom.requestFullscreen();
  } else if (dom.webkitRequestFullscreen) {
    dom.webkitRequestFullscreen();
  } else if (dom.mozRequestFullScreen) {
    dom.mozRequestFullScreen();
  } else {
    dom.msRequestFullscreen();
  }
}

function exitFullScreen() {
  if (!document.fullscreen) {
    return;
  }
  if (document.exitFullscreen) {
    document.exitFullscreen();
  } else if (document.msExitFullscreen) {
    document.msExitFullscreen();
  } else if (document.mozCancelFullScreen) {
    document.mozCancelFullScreen();
  } else if (document.webkitExitFullscreen) {
    document.webkitExitFullscreen();
  }
}
function getCurrentFullScreen() {
  const isFullScreen = document.webkitFullscreenElement
    || document.mozFullScreenElement
    || document.msFullscreenElement;
  return Boolean(isFullScreen);
}

export default function useFullScreen(target, onFullScreenChange, customClassName = 'fullScrenn') {
  const [isFullScreen, setIsFullScreen] = useState(false);
  const handleChangeFullScreen = () => {
    const element = typeof target === 'function' ? target() : target;
    const currentFullScreen = getCurrentFullScreen();
    setTimeout(() => {
      setIsFullScreen(currentFullScreen); // 推迟设置state，防止出现state更改完成，但是没有渲染完成，可能导致的问题
    });
    if (currentFullScreen) {
      element.classList.add(customClassName);
    } else {
      element.classList.remove(customClassName);
    }
    if (onFullScreenChange) {
      onFullScreenChange(currentFullScreen);
    }
  };

  const toggleFullScreen = () => {
    const currentFullScreen = getCurrentFullScreen();
    const element = typeof target === 'function' ? target() : target;
    if (currentFullScreen) {
      exitFullScreen();
    } else {
      toFullScreen(element);
    }
  };
  useEffect(() => {
    document.addEventListener('fullscreenchange', handleChangeFullScreen);
    document.addEventListener('webkitfullscreenchange', handleChangeFullScreen);
    document.addEventListener('mozfullscreenchange', handleChangeFullScreen);
    document.addEventListener('MSFullscreenChange', handleChangeFullScreen);
    return function cleanup() {
      if (isFullScreen) {
        exitFullScreen(); // 组件卸载时如果是全屏，就自动退出全屏
        setTimeout(() => {
          setIsFullScreen(false); // 推迟设置state，防止出现state更改完成，但是没有渲染完成，可能导致的问题
        });
        const element = typeof target === 'function' ? target() : target;
        element.classList.remove(customClassName);
      }
      document.removeEventListener('fullscreenchange', handleChangeFullScreen);
      document.removeEventListener('webkitfullscreenchange', handleChangeFullScreen);
      document.removeEventListener('mozfullscreenchange', handleChangeFullScreen);
      document.removeEventListener('MSFullscreenChange', handleChangeFullScreen);
    };
  }, [isFullScreen]);
  return [isFullScreen, toggleFullScreen];
}
