/* eslint-disable react/require-default-props */
import type { Gantt, GanttRef } from '@choerodon/gantt';
import { GanntMoveWrap } from '@choerodon/gantt';

import classNames from 'classnames';
import React, { useCallback, useMemo } from 'react';
import { Tooltip } from 'choerodon-ui/pro';
import { merge, set } from 'lodash';
import dayjs from 'dayjs';
import { observer, useComputed } from 'mobx-react-lite';
import styles from './Bar.less';

interface IGanttBaseBarRange {
  start: Gantt.DateWithWidth, end: Gantt.DateWithWidth,
}
interface IGanttBaseBarProps {
  startDate?: Gantt.DateWithWidth
  ganttRef: React.RefObject<GanttRef>
  progressCount?: { total: number, completed: number }
  dashMoveDateRange?: Partial<IGanttBaseBarRange>
  fillMoveDateRange?: Partial<IGanttBaseBarRange>
  dashDateRange?: IGanttBaseBarRange & { color?: string }
  fillDateRange?: IGanttBaseBarRange & { completedColor?: string, unCompletedColor?: string }
  /** 有截止日期才会有延期 */
  deadline?: string | Gantt.DateWithWidth
  tooltipTitle?: boolean | React.ReactNode
  bar: Gantt.Bar
  width: number
  height: number
}
/**
 * 甘特图基础Bar
 * @param props
 * @returns
 */
function GanttBaseBar(props: React.PropsWithChildren<IGanttBaseBarProps>) {
  const {
    width, height, tooltipTitle, progressCount, fillDateRange, dashDateRange, startDate, ganttRef, bar,
    fillMoveDateRange, dashMoveDateRange,
  } = props;
  const isAddUnitDay = useCallback(({ start, end }: IGanttBaseBarRange) => start.key !== startDate?.key, [startDate?.key]);
  const dashStyle = useMemo(() => {
    if (!dashDateRange) {
      return {};
    }
    const dashWidth = dashDateRange.end.width - dashDateRange.start.width + (isAddUnitDay(dashDateRange) && dashDateRange.end.dragFloat === 'right' ? dashDateRange.end.unitWidth : 0);
    return {
      width: dashWidth,
      marginLeft: Math.max(0, dashDateRange.start.dragLeft),
      height,
      display: dashWidth <= 0 ? 'none' : undefined,
      borderColor: dashDateRange.color,
    } as React.CSSProperties;
  }, [dashDateRange, height, isAddUnitDay]);
  const deadline = useMemo(() => {
    if (!props.deadline || !fillDateRange || !dashDateRange) {
      return undefined;
    }
    return typeof props.deadline === 'string' ? { width: Math.max(0, ganttRef.current?.getWidthByDate(dayjs(dashDateRange.end.value), dayjs(props.deadline)) || 0), value: props.deadline, today: true } : {
      width: Math.max(0, props.deadline.width - (dashDateRange.end.width as number || 0)),
    };
  }, [dashDateRange, fillDateRange, ganttRef, props.deadline]);
  const fillStyle = useMemo(() => {
    if (!fillDateRange) {
      return { width: 0 };
    }
    const endWidth = fillDateRange.end.width + (deadline?.today ? deadline?.width || 0 : 0);
    const fillWidth = endWidth - fillDateRange.start.width + (isAddUnitDay(fillDateRange) && fillDateRange.end.dragFloat === 'right' ? fillDateRange.end.unitWidth : 0);
    return {
      width: fillWidth, minWidth: fillWidth, marginLeft: Math.max(0, fillDateRange.start.dragLeft), backgroundColor: fillDateRange.unCompletedColor,
    } as React.CSSProperties;
  }, [deadline?.today, deadline?.width, fillDateRange, isAddUnitDay]);
  const progressStyle = useMemo(() => {
    const progress = ({
      completed: { flex: 1, backgroundColor: fillDateRange?.completedColor } as React.CSSProperties,
      unCompleted: { flex: 0 } as React.CSSProperties,
    });
    progressCount?.total && merge(progress, {
      completed: { flex: progressCount.completed },
      unCompleted: { flex: progressCount.total - (progressCount.completed || 0) },
    });
    return progress;
  }, [fillDateRange?.completedColor, progressCount?.completed, progressCount?.total]);

  const handleTooltipMouseLeave = useCallback(() => Tooltip.hide(), []);
  const handleTooltipMouseEnter = useCallback((e) => Tooltip.show(e.target, {
    onPopupAlign: (source: HTMLDivElement) => {
      // eslint-disable-next-line no-param-reassign
      source.style.left = `${e.clientX}px`;
    },
    title: tooltipTitle,
  }), [tooltipTitle]);
  return (
    <div className={styles.wrap} style={{ width, height }}>
      <div
        className={styles.body}
        onMouseMove={handleTooltipMouseEnter}
        onMouseEnter={handleTooltipMouseEnter}
        onMouseLeave={handleTooltipMouseLeave}
      >
        <GanntMoveWrap data={bar} startDate={dashMoveDateRange?.start} endDate={dashMoveDateRange?.end}>
          <div id="ganttBar" className={styles.dash} style={dashStyle} />
        </GanntMoveWrap>
        <GanntMoveWrap data={bar} startDate={fillMoveDateRange?.start} endDate={fillMoveDateRange?.end}>
          <div className={styles.fill} style={fillStyle}>
            <div className={styles.fill_progress}>
              <div style={progressStyle.completed} />
              <div style={progressStyle.unCompleted} />
            </div>
            <div className={classNames(styles.delay, styles.fill_delay)} style={{ width: deadline?.width || 0 }} />
          </div>
        </GanntMoveWrap>
      </div>
      <div className={styles.delay} />
    </div>
  );
}
export default observer(GanttBaseBar);
