import { usePersistFn } from 'ahooks';
import { Tooltip } from 'choerodon-ui/pro';
import type { TooltipProps } from 'choerodon-ui/pro/lib/tooltip/Tooltip';
import { useRef } from 'react';

function useTooltip(config?: { tooltip: TooltipProps }) {
  const titleRef = useRef<TooltipProps['title']>(config?.tooltip.title);
  titleRef.current = config?.tooltip.title;
  const handleTooltipMouseEnter = usePersistFn(
    (e) => Tooltip.show(e.target, {
      title: titleRef.current,
      placement: 'topLeft',
      ...config?.tooltip,
    }),
  );
  const handleTooltipMouseLeave = usePersistFn(() => Tooltip.hide());
  return { onMouseEnter: handleTooltipMouseEnter, onMouseLeave: handleTooltipMouseLeave };
}
export default useTooltip;
