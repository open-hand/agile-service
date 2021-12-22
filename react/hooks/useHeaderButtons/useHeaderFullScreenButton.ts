import { useRef } from 'react';
import { BasicTarget } from 'ahooks/lib/utils/dom';
import { ButtonProps } from 'choerodon-ui/pro/lib/button/Button';

import useFullScreen from '@/common/useFullScreen';
import useTooltip from '../useTooltip';

interface IHeaderFullScreenButtonHookConfig {
  /** 附加隐藏相关样式 来进行局部全屏，避免依靠在body下的弹窗无法使用 */
  appendCustomClassName?: string
  fullDom?: BasicTarget
}
interface IGeneralHeaderFullScreenButtonHookConfig extends IHeaderFullScreenButtonHookConfig {
  /**  @default true   */
  onlyIcon?: boolean
  /**  @default true   */

  tooltip?: boolean
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
/**
 * 通用全屏按钮配置hook
 * @param param0
 * @returns
 */
export function useGeneralHeaderFullScreenButton<TD extends HTMLElement = any>({ tooltip = true, onlyIcon = true, ...otherConfigProps }: IGeneralHeaderFullScreenButtonHookConfig) {
  const [data, headerComponentProps] = useHeaderFullScreenButton<TD>(otherConfigProps);
  const tooltipProps = useTooltip({ tooltip: { title: tooltip ? headerComponentProps.tooltipsConfig.title : null } });
  const componentProps: ButtonProps = {
    ...headerComponentProps, ...tooltipProps, children: onlyIcon ? undefined : headerComponentProps.tooltipsConfig.title, onClick: headerComponentProps.handler,
  };
  return [data, componentProps] as const;
}
type IHeaderFullScreenButtonComponentProps = ReturnType<typeof useHeaderFullScreenButton>[1]
export type { IHeaderFullScreenButtonComponentProps };
export default useHeaderFullScreenButton;
