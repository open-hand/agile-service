/* eslint-disable react/require-default-props */
import type { Gantt, GanttRef } from '@choerodon/gantt';
import { GanntMoveWrap } from '@choerodon/gantt';
import { toJS } from 'mobx';
import classNames from 'classnames';
import React, { useCallback, useMemo } from 'react';
import { merge } from 'lodash';
import dayjs from 'dayjs';
import { observer, useComputed } from 'mobx-react-lite';
import styles from './Bar.less';
import STATUS_COLOR from '@/constants/STATUS_COLOR';
import useFollowMouseTooltip from '@/hooks/useFollowMouseTooltip';

interface IWrapGanttBaseBarFindOptions {
  type: 'issue'
}
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
  dragging?: boolean
}
/**
 * 甘特图基础Bar
 * @param props
 * @returns
 */
function GanttBaseBar(props: React.PropsWithChildren<IGanttBaseBarProps>) {
  const {
    width, height, tooltipTitle, progressCount, fillDateRange, dashDateRange, startDate, ganttRef, bar, dragging,
    fillMoveDateRange, dashMoveDateRange,
  } = props;
  const isAddUnitDay = useCallback(({ start, end }: IGanttBaseBarRange) => start.key !== startDate?.key, [startDate?.key]);
  const dashStyle = useMemo(() => {
    if (!dashDateRange) {
      return { display: 'none' };
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
    if (typeof props.deadline === 'string') {
      return dragging ? undefined : { width: Math.max(0, ganttRef.current?.getWidthByDate(dayjs(fillDateRange.end.value).set('hour', 23).set('minute', 59).set('second', 59), dayjs(props.deadline)) || 0), value: props.deadline, today: true };
    }
    return {
      width: fillDateRange.start.dragLeft > dashDateRange.end.width ? '100%' : Math.max(0, props.deadline.width - (dashDateRange.end.width as number || 0)),
    };
  }, [dashDateRange, dragging, fillDateRange, ganttRef, props.deadline]);
  const fillStyle = useMemo(() => {
    if (!fillDateRange) {
      return { width: 0 };
    }
    const fillWidth = fillDateRange.end.width - fillDateRange.start.width + (deadline?.today ? deadline?.width || 0 : 0)
      + (!deadline?.today && isAddUnitDay(fillDateRange) && fillDateRange.end.dragFloat === 'right' ? fillDateRange.end.unitWidth : 0);
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

  const [, targetProps] = useFollowMouseTooltip({ tooltipTitle });
  return (
    <div className={styles.wrap} style={{ width, height }}>
      <div
        className={styles.body}
        {...targetProps}
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
/**
 * 为甘特图配置快速配置时间条
 * 会使用issue下的时间条展示规则进行问题展示（不包含进度 progressCount 的配置）
 * @param Element
 * @param options
 */
export function wrapGanttBaseBarFindDate(Element: React.FC<IGanttBaseBarProps>, options?: IWrapGanttBaseBarFindOptions) {
  function GanttBaseBarHOC(props: React.PropsWithChildren<IGanttBaseBarProps>) {
    const { bar } = props;
    const {
      record: issue, loading, stepGesture, dateMaps, startDateKey,
    } = bar;
    const [color1, color2] = useComputed(() => {
      const statusType = issue.statusVO.type;
      return STATUS_COLOR[statusType as keyof typeof STATUS_COLOR];
    }, [issue.statusVO.type]);
    const estimatedStartTime = dateMaps.get('estimatedStartTime');
    const estimatedEndTime = dateMaps.get('estimatedEndTime');
    const actualStartTime = dateMaps.get('actualStartTime');
    const actualEndTime = dateMaps.get('actualEndTime');
    const dashDateRange = useComputed(() => (estimatedStartTime ? {
      start: toJS(estimatedStartTime),
      end: toJS(estimatedEndTime || estimatedStartTime),
      color: color1,
    } : undefined), [estimatedStartTime, estimatedStartTime?.value, estimatedEndTime, estimatedEndTime?.value, color1]);
    const fillDateRange = useComputed(() => {
      if (actualStartTime) {
        const start = toJS(actualStartTime);
        const end = toJS(actualEndTime || dashDateRange?.end || actualStartTime);
        const startKey = start.width > end.width ? 'end' : 'start';

        return {
          [startKey as 'start']: start,
          [startKey === 'start' ? 'end' : 'start' as 'end']: end,
          completedColor: color1,
          unCompletedColor: color2,
          color: color1,
        };
      }

      return undefined;
    }, [actualStartTime?.value, actualEndTime?.value, dashDateRange?.end, color1, color2, dashDateRange]);
    const deadline = useComputed(() => {
      if (actualEndTime?.value) {
        return toJS(actualEndTime);
      }
      return (estimatedStartTime && estimatedEndTime ? dayjs().set('hour', 10).set('minute', 0).set('second', 0)
        .format('YYYY-MM-DD HH:mm:ss') : undefined);
    }, [actualEndTime, estimatedEndTime, estimatedStartTime, actualEndTime?.value]);
    return (
      <Element
        startDate={dateMaps.get(startDateKey)}
        deadline={deadline}
        fillDateRange={fillDateRange}
        dashDateRange={dashDateRange}
        dragging={stepGesture === 'moving'}
        fillMoveDateRange={{ start: actualStartTime, end: actualEndTime }}
        dashMoveDateRange={{ start: estimatedStartTime, end: estimatedEndTime }}
        {...props}
      />
    );
  }
  if (options?.type === 'issue') {
    const ObserverGanttBaseBarHOC = observer(GanttBaseBarHOC);
    ObserverGanttBaseBarHOC.displayName = `${Element.displayName}For${options?.type}`;
    return ObserverGanttBaseBarHOC;
  }
  return Element;
}
export default observer(GanttBaseBar);
