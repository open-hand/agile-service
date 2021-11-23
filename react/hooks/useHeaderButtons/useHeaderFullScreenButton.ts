import { useRef } from 'react';
import { BasicTarget } from 'ahooks/lib/utils/dom';
import useFullScreen from '@/common/useFullScreen';

interface IHeaderFullScreenButtonHookConfig {
    /** 附加隐藏相关样式 来进行局部全屏，避免依靠在body下的弹窗无法使用 */
    appendCustomClassName?: string
    fullDom?: BasicTarget
}
function useHeaderFullScreenButton<TD extends HTMLElement = any>(config?: IHeaderFullScreenButtonHookConfig) {
  const fullDomRef = useRef<TD>(null);

  const [isFullScreen, toggleFullScreen] = useFullScreen(config?.fullDom ?? (() => fullDomRef.current || document.body), () => { }, config?.appendCustomClassName);
  const componentProps = {
    icon: isFullScreen ? 'fullscreen_exit' : 'zoom_out_map',
    iconOnly: true,
    display: true,
    handler: () => {
      toggleFullScreen();
    },
    tooltipsConfig: {
      title: isFullScreen ? '退出全屏' : '全屏',
    },
  };
  return [{
    fullDomRef,
    isFullScreen,
  }, componentProps] as const;
}
export default useHeaderFullScreenButton;
