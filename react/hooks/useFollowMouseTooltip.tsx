import React, { useCallback, useRef } from 'react';
import { Tooltip } from 'choerodon-ui/pro';
import { useCreation, usePersistFn } from 'ahooks';
import { isEqual } from 'lodash';

interface IHookFollowMouseTooltipProps {
    tooltipTitle?: React.ReactNode
}
/**
 * 跟随鼠标移动的tooltip
 * @param param0
 * @returns
 */
function useFollowMouseTooltip({ tooltipTitle }: IHookFollowMouseTooltipProps) {
  const tooltipPropsRef = useRef<any>();
  const targetRef = useRef<any>();
  const handleTooltipMouseLeave = useCallback(() => Tooltip.hide(), []);
  const handleTooltipMouseEnter = useCallback((e) => {
    targetRef.current = e.target;
    Tooltip.show(e.target, {
      onPopupAlign: (source: HTMLDivElement, align: object, target: Node | Window, translate: { x: number; y: number }) => {
        const left = Math.max(0, (e.clientX || 0) - 21);
        left && source.style.setProperty('left', `${left}px`);
      },
      autoAdjustOverflow: true,
      arrowPointAtCenter: true,
      title: tooltipTitle,
      placement: 'topLeft',
      ...tooltipPropsRef.current,
    });
  }, [tooltipTitle]);
  const handleUpdateTooltip = usePersistFn((tooltipProps: { title?: any, [key: string]: any }) => {
    if (!isEqual(tooltipProps, tooltipPropsRef.current)) {
      tooltipPropsRef.current = tooltipProps;
      targetRef.current && handleTooltipMouseEnter(targetRef.current);
    }
  });
  const actions = useCreation(() => ({ update: handleUpdateTooltip }), []);
  return [{ actions }, { onMouseMove: handleTooltipMouseEnter, onMouseEnter: handleTooltipMouseEnter, onMouseLeave: handleTooltipMouseLeave }] as const;
}
export default useFollowMouseTooltip;
