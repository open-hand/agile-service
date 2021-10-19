import React, {
  useContext, useMemo, useRef, useEffect,
} from 'react';
import {
  observer, useComputed, useObservable, Observer, useObserver,
} from 'mobx-react-lite';
import dayjs, { Dayjs } from 'dayjs';
import { Tooltip } from 'choerodon-ui/pro';
import { GanttProps, Gantt } from '@choerodon/gantt';
import { find } from 'lodash';
import STATUS_COLOR from '@/constants/STATUS_COLOR';
import Context from '../../context';
import styles from './index.less';
import type { GanttIssue } from '../../types';

interface GanttBarProps {
  type: string
  bar: Gantt.Bar<GanttIssue>
  width: number
  height: number
  dateKeyRange: [string, string]
  onClick: GanttProps<GanttIssue>['onBarClick']
}
function format(h: number) {
  if (h >= 24) {
    const days = Math.floor(h / 24);
    const hours = h % 24;

    return `${days}天${hours > 0 ? `${hours}小时` : ''}`;
  }
  return `${h}小时`;
}
const GanttBar: React.FC<GanttBarProps> = ({
  type, bar, width, height, onClick, dateKeyRange,
}) => {
  const { store } = useContext(Context);
  const { ganttRef } = store;
  const estimateRef = useRef<HTMLDivElement>(null);
  const {
    record: issue, loading, stepGesture, task, middleWidthList, dateMaps,
  } = bar;

  const statusType = issue.statusVO.type;
  const subTasks = issue.children ? issue.children.filter((i) => i.issueTypeVO?.typeCode === 'sub_task') : [];
  const hasChildren = subTasks && subTasks.length > 0;
  const totalCount = subTasks?.length || 0;
  const completeCount = subTasks?.filter((item) => item.completed).length || 0;
  // @ts-ignore
  const [color1, color2] = STATUS_COLOR[statusType];
  const percent = totalCount ? completeCount / totalCount : 0;
  let diff = 0;
  const delayDiff = 0;
  if (issue.estimatedStartTime && issue.estimatedEndTime) {
    // 延期
    // if (dayjs(issue.estimatedEndTime).isBefore(dayjs()) && !issue.completed) {
    //   color1 = '#FF5C6A';
    //   color2 = '#FFBAC0';
    // }
    diff = dayjs(issue.estimatedEndTime).diff(issue.estimatedStartTime, 'hour');
  }
  const actualStartTime = useComputed(() => dateMaps.get('actualStartTime') || { width: 0 } as Gantt.MiddleDateWithWidth, [dateMaps]);
  const actualEndTime = useComputed(() => dateMaps.get('actualEndTime') || { width: 0 } as Gantt.MiddleDateWithWidth, [dateMaps]);
  const estimatedStartTime = useComputed(() => dateMaps.get('estimatedStartTime') || { width: 0 } as Gantt.MiddleDateWithWidth, [dateMaps]);
  const estimatedEndTime = useComputed(() => dateMaps.get('estimatedEndTime') || { width: 0 } as Gantt.MiddleDateWithWidth, [dateMaps]);

  const actualTime = useComputed(() => {
    let fragmentWidth = actualEndTime.width - actualStartTime.width;
    let left = actualStartTime.width;
    if (actualStartTime.width > 0) {
      fragmentWidth += (actualEndTime as Gantt.MiddleDateWithWidth).unitWidth;
      left -= (actualEndTime as Gantt.MiddleDateWithWidth).unitWidth;
    }
    return { width: fragmentWidth, left };
  }, [actualStartTime, actualEndTime, actualStartTime.width]);
  const estimateTime = useComputed(() => {
    let fragmentWidth = estimatedEndTime.width - estimatedStartTime.width;
    let left = estimatedStartTime.width;

    if (estimatedStartTime.width > 0) {
      fragmentWidth += (estimatedEndTime as Gantt.MiddleDateWithWidth).unitWidth;
      left -= (estimatedEndTime as Gantt.MiddleDateWithWidth).unitWidth;
    }
    return { width: fragmentWidth, left };
  }, [estimatedStartTime, estimatedStartTime.width, estimatedEndTime, estimatedEndTime.width]);
  const delayWidth = (() => {
    if (!estimatedEndTime.value || !actualEndTime.value || actualEndTime.value === '' || loading) {
      return 0;
    }
    const dWidth = ganttRef.current?.getWidthByDate(dayjs(estimatedEndTime.value), dayjs(actualEndTime.value));
    return dWidth && dWidth > 0 ? dWidth : 0;
  })();
  const delayVisible = stepGesture !== 'moving' && !loading;
  return (
    <>
      <Tooltip
        // @ts-ignore
        getPopupContainer={(t) => document.getElementsByClassName('gantt-chart')[0] as HTMLElement}
        title={(
          <div>
            {issue.summary}
            <div>
              状态：
              {issue.statusVO.name}
            </div>
            <div>
              持续时间：
              {format(diff)}
            </div>
            {delayDiff > 0 && (
              <div>
                逾期：
                {format(delayDiff)}
              </div>
            )}
            {type !== 'assignee' && hasChildren && (
              <div>
                当前进度：
                {`${Math.round(percent * 100 * 100) / 100}%`}
              </div>
            )}
            <div>
              预计开始：
              {issue.estimatedStartTime}
            </div>
            <div>
              预计结束：
              {issue.estimatedEndTime}
            </div>
          </div>
        )}
      >
        <div style={{
          width: width - delayWidth + 1,
          height,
          padding: '.02rem',
          // backgroundColor: color2,
          // borderColor: color1,
          display: 'flex',
          position: 'relative',
          borderRadius: delayVisible && delayWidth > 0 ? '2px 0 0 2px' : '2px',
          overflow: 'hidden',
        }}
        >
          <div
            ref={estimateRef}
            id="ganttBar"
            style={{
              marginLeft: estimateTime.left,
              width: estimateTime.width - 1,
              height,
              borderColor: color1,
            }}
            className={styles.estimate}
          />
          <div className={styles.actual} style={{ width: actualTime.width - delayWidth + 1, marginLeft: actualTime.left, backgroundColor: color2 }}>
            <div style={{ flex: totalCount > 0 ? completeCount : 1, backgroundColor: color1 }} />
            <div style={{ flex: totalCount > 0 ? totalCount - completeCount : 0 }} />
          </div>
        </div>
        {delayVisible && (
          <div
            role="none"
            onMouseDown={(e) => {
              e.stopPropagation();
            }}
            onClick={(e) => {
              e.stopPropagation();
              onClick && onClick(bar.record);
            }}
            className={styles.delay}
            style={{ width: delayWidth, marginLeft: width - delayWidth }}
          />
        )}
      </Tooltip>
    </>
  );
};
export default observer(GanttBar);
