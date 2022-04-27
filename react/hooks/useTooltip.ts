import { usePersistFn } from 'ahooks';
import { Tooltip } from 'choerodon-ui/pro';
import type { TooltipProps } from 'choerodon-ui/pro/lib/tooltip/Tooltip';
import { useRef, useMemo } from 'react';

function useTooltip(config?: { tooltip: TooltipProps }) {
  const tooltipPropsRef = useRef<TooltipProps>();
  tooltipPropsRef.current = config?.tooltip;
  const handleTooltipMouseEnter = usePersistFn(
    (e, tooltipProps?: TooltipProps, duration = 100) => Tooltip.show(e.target, {
      placement: 'topLeft',
      ...tooltipPropsRef.current,
      ...tooltipProps,
    }, duration),
  );
  const handleTooltipMouseLeave = usePersistFn((duration = 100) => Tooltip.hide(duration));
  return useMemo(() => ({ onMouseEnter: handleTooltipMouseEnter, onMouseLeave: handleTooltipMouseLeave }), [handleTooltipMouseEnter, handleTooltipMouseLeave]);
}
export default useTooltip;
