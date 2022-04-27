import { usePersistFn } from 'ahooks';
import { Tooltip } from 'choerodon-ui/pro';
import type { TooltipProps } from 'choerodon-ui/pro/lib/tooltip/Tooltip';
import { isEqual } from 'lodash';
import { useRef, useMemo } from 'react';

function useTooltip(config?: { tooltip: TooltipProps }) {
  const tooltipPropsRef = useRef<TooltipProps>();
  const targetRef = useRef<any>();
  tooltipPropsRef.current = config?.tooltip;
  const handleTooltipMouseEnter = usePersistFn(
    (e, tooltipProps?: TooltipProps, duration = 100) => {
      targetRef.current = e.target;
      return Tooltip.show(targetRef.current, {
        placement: 'topLeft',
        ...tooltipPropsRef.current,
        ...tooltipProps,
      }, duration);
    },
  );
  const handleTooltipMouseLeave = usePersistFn((duration = 100) => Tooltip.hide(duration));
  const handleUpdateTooltip = usePersistFn((tooltipProps: Partial<TooltipProps>) => {
    if (!isEqual(tooltipProps, tooltipPropsRef.current)) {
      tooltipPropsRef.current = tooltipProps;
      targetRef.current && handleTooltipMouseEnter(targetRef.current);
    }
  });
  return useMemo(() => ({ onMouseEnter: handleTooltipMouseEnter, onMouseLeave: handleTooltipMouseLeave, updateTooltipProps: handleUpdateTooltip }), [handleTooltipMouseEnter, handleTooltipMouseLeave, handleUpdateTooltip]);
}
export default useTooltip;
