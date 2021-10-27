import React, {
  useContext, useMemo, useRef, useEffect, useCallback,
} from 'react';
import {
  observer, useComputed, useObservable, Observer, useObserver,
} from 'mobx-react-lite';
import dayjs, { Dayjs } from 'dayjs';
import { Tooltip } from 'choerodon-ui/pro';
import type { GanttProps, Gantt } from '@choerodon/gantt';
import { GanntMoveWrap } from '@choerodon/gantt';

import { find, set } from 'lodash';
import { TooltipProps } from 'choerodon-ui/pro/lib/tooltip/Tooltip';
import { useThrottleFn } from 'ahooks';
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
  const actualStartTime = useComputed(() => dateMaps.get('actualStartTime') || { width: 0 } as Gantt.DateWithWidth, [dateMaps]);
  const actualEndTime = useComputed(() => dateMaps.get('actualEndTime') || { width: 0 } as Gantt.DateWithWidth, [dateMaps]);
  const estimatedStartTime = useComputed(() => dateMaps.get('estimatedStartTime') || { width: 0 } as Gantt.DateWithWidth, [dateMaps]);
  const estimatedEndTime = useComputed(() => dateMaps.get('estimatedEndTime') || { width: 0 } as Gantt.DateWithWidth, [dateMaps]);

  // 这里的返回的宽度都在可拖拽区域
  const actualTime = useComputed(() => {
    if (estimatedEndTime.width === 0 || (actualEndTime.width === 0 && !actualStartTime.value)) {
      return { width: 0, left: 0 };
    }
    let fragmentWidth = actualEndTime.width - actualStartTime.width;
    let left = actualStartTime.width;
    if (actualEndTime.width === 0) {
      fragmentWidth = estimatedEndTime.width - actualStartTime.width;
      left = fragmentWidth > 0 ? (actualStartTime as Gantt.DateWithWidth).width : estimatedEndTime.width;
    }
    let delay = Math.max(0, Math.abs(actualEndTime.width - estimatedEndTime.width));
    console.log('fragmentWidth', fragmentWidth);
    if (fragmentWidth > 0) {
      delay = 0;
    }
    if (fragmentWidth > 0 && actualStartTime.width > estimatedEndTime.width) {
      fragmentWidth += actualStartTime.unitWidth;
      left -= actualStartTime.unitWidth;
      delay = fragmentWidth;
    }

    fragmentWidth = Math.abs(fragmentWidth);
    delay = Math.min(fragmentWidth, delay);

    // left = Math.max(0, left);
    return {
      width: fragmentWidth, left, delayWidth: delay, processWidth: fragmentWidth - delay,
    };
  }, [actualStartTime, actualEndTime.width, estimatedEndTime.width, actualStartTime.width]);
  const estimateTime = useComputed(() => {
    let fragmentWidth = estimatedEndTime.width - estimatedStartTime.width;
    let left = estimatedStartTime.width;

    if (estimatedStartTime.width > 0) {
      fragmentWidth += (estimatedEndTime as Gantt.DateWithWidth).unitWidth;
      left -= (estimatedEndTime as Gantt.DateWithWidth).unitWidth;
    }
    return { width: fragmentWidth, left };
  }, [estimatedStartTime, estimatedStartTime.width, estimatedEndTime, estimatedEndTime.width]);
  // 此宽度不可操作
  const delayWidth = (() => {
    if (!estimatedEndTime.value || actualEndTime.value || loading) {
      return 0;
    }
    const dWidth = ganttRef.current?.getWidthByDate(dayjs(estimatedEndTime.value), dayjs());
    return dWidth && dWidth > 0 ? dWidth : 0;
  })();
  const delayVisible = !(issue.statusVO.type === 'done') && !actualEndTime.width && (stepGesture !== 'moving') && !loading;
  const operateWidth = actualEndTime.value ? width - delayWidth : width;
  const actualTimeWidth = actualTime.width; // actualTime.width - delayWidth + 1;

  const handleTooltipMouseEnter: React.MouseEventHandler<HTMLDivElement> = useCallback(
    (e) => Tooltip.show(e.target, {
      onPopupAlign: (source: HTMLDivElement, align, target, translate) => {
        // eslint-disable-next-line no-param-reassign
        source.style.left = `${e.clientX}px`;
      },
      title: (
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
            {issue.estimatedStartTime?.split(' ')[0] || '-'}
          </div>
          <div>
            预计结束：
            {issue.estimatedEndTime?.split(' ')[0] || '-'}
          </div>
          <div>
            实际开始：
            {issue.actualStartTime?.split(' ')[0] || '-'}
          </div>
          <div>
            实际结束：
            {issue.actualEndTime?.split(' ')[0] || '-'}
          </div>
        </div>
      ),
      placement: 'topLeft',
    } as TooltipProps),
    [diff, hasChildren, issue.actualEndTime, issue.actualStartTime, issue.estimatedEndTime, issue.estimatedStartTime, issue.statusVO.name, issue.summary, percent, type],
  );
  const handleTooltipMouseLeave = useCallback(() => Tooltip.hide(), []);
  return (
    <div style={{ width }}>
      <div
        onMouseMove={handleTooltipMouseEnter}
        onMouseEnter={handleTooltipMouseEnter}
        onMouseLeave={handleTooltipMouseLeave}
        style={{
          width: operateWidth,
          height,
          padding: '.02rem',
          // backgroundColor: color2,
          // borderColor: color1,
          display: 'flex',
          position: 'relative',
          borderRadius: delayVisible && delayWidth > 0 ? '2px 0 0 2px' : '2px',
          // overflow: 'hidden',
        }}
      >
        <GanntMoveWrap data={bar} startDate={estimatedStartTime} endDate={estimatedEndTime}>
          <div
            ref={estimateRef}
            id="ganttBar"
            style={{
              marginLeft: estimateTime.left,
              width: estimateTime.width,
              display: estimateTime.width <= 0 ? 'none' : undefined,
              height,
              borderColor: color1,
            }}
            className={styles.estimate}
          />
        </GanntMoveWrap>
        <GanntMoveWrap data={bar} startDate={actualStartTime} endDate={actualEndTime}>
          <div
            className={styles.actual}
            style={{ width: actualTimeWidth, marginLeft: actualTime.left }}
          >
            <div className={styles.actual_process} style={{ width: actualTime.processWidth, display: actualTime.processWidth ? 'flex' : 'none', background: color2 }}>
              <div style={{ flex: totalCount > 0 ? completeCount : 1, backgroundColor: color1 }} />
              <div style={{ flex: totalCount > 0 ? totalCount - completeCount : 0 }} />
            </div>
            <div className={styles.delay} style={{ width: actualTime.delayWidth }} />
          </div>
        </GanntMoveWrap>
      </div>
      {delayVisible && (
        <div
          role="none"
          onMouseDown={(e) => {
            e.stopPropagation();
          }}
          onMouseEnter={handleTooltipMouseEnter}
          onMouseLeave={handleTooltipMouseLeave}
          onClick={(e) => {
            e.stopPropagation();
          }}
          className={styles.delay}
          style={{ width: delayWidth, marginLeft: operateWidth }}
        />
      )}
    </div>
  );
};
export default observer(GanttBar);
