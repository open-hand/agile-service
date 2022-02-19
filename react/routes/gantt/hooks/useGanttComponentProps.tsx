import React, { useCallback } from 'react';
import type { GanttProps } from '@choerodon/gantt';
import {
  Tooltip, Icon,
} from 'choerodon-ui/pro';
import { useCreation, usePersistFn } from 'ahooks';
import useGanttGetExpandIcon from './useGanttGetExpandIcon';
import localCacheStore from '@/stores/common/LocalCacheStore';

type IHookUseGanttGanttBaseProps = Partial<GanttProps> & Pick<GanttProps, 'data' | 'columns' |
    'onUpdate' | 'middleDateKeys' | 'startDateKey' | 'endDateKey'>
interface IHookUseGanttComponentProps extends IHookUseGanttGanttBaseProps {

}
interface IHookUseGanttComponentConfig {
    invalidBarTooltip?: boolean | React.ReactNode,
    /** 自动保存表格宽度缓存code @default 'agile.gantt.table.width' */
    autoCacheTableWidthCode?: string
}

/**
 * 提供一个敏捷甘特图所需配置Props
 * @param param0
 * @returns
 */
function useGanttComponentProps(props: IHookUseGanttComponentProps, config?: IHookUseGanttComponentConfig): [GanttProps] {
  const { invalidBarTooltip = true, autoCacheTableWidthCode = 'agile.gantt.table.width' } = config || {};
  const expandIcon = useGanttGetExpandIcon();
  const handleTooltipMouseEnter = useCallback(
    (e, title?: string) => Tooltip.show(e.target, {
      title: title ?? '点击并拖动以设置预计开始、结束时间。',
      placement: 'topLeft',
    }),
    [],
  );
  const handleResizeWidth: GanttProps['onResizeWidth'] = usePersistFn((tableWidth) => {
    localCacheStore.unPrefix().setItem(autoCacheTableWidthCode as any, tableWidth);
  });
  const handleTooltipMouseLeave = useCallback(() => Tooltip.hide(), []);
  const renderInvalidBar: GanttProps['renderInvalidBar'] = useCallback((element, barInfo) => (
    <span onMouseEnter={handleTooltipMouseEnter} onMouseLeave={handleTooltipMouseLeave}>
      {element}
    </span>
  ), [handleTooltipMouseEnter, handleTooltipMouseLeave]);
  const renderBarThumb: GanttProps['renderBarThumb'] = useCallback((record, t) => (
    <div
      role="none"
      className="c7n-gantt-thumb-icon"
    >
      {t === 'left' ? <Icon type="navigate_before" /> : <Icon type="navigate_next" />}
    </div>
  ), []);
  const constantProps: Partial<GanttProps> = useCreation(() => ({
    showBackToday: false,
    showUnitSwitch: false,
    tableCollapseAble: false,
    scrollTop: {
      right: -4,
      bottom: 8,
    },
    tableIndent: 20,
    expandIcon,
    renderBarThumb,
    renderInvalidBar: invalidBarTooltip ? renderInvalidBar : undefined,
    defaultTableWidth: Number(localCacheStore.unPrefix().getItem(autoCacheTableWidthCode as any)) || undefined,
    onResizeWidth: handleResizeWidth,
    rowHeight: 34,
    barHeight: 13,
    startDateKey: 'estimatedStartTime',
    endDateKey: 'estimatedEndTime',
    middleDateKeys: [{ key: 'actualStartTime', maxDateKey: 'actualEndTime', ignoreCheckDateKeys: ['actualEndTime'] }, { key: 'actualEndTime', minDateKey: 'actualStartTime' }],
  }), [invalidBarTooltip]);
  return [{
    ...constantProps,
    ...props,
  }];
}
export default useGanttComponentProps;
