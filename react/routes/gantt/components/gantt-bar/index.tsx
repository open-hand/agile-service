import React, { useContext } from 'react';
import { observer } from 'mobx-react-lite';
import dayjs, { Dayjs } from 'dayjs';
import { Tooltip } from 'choerodon-ui/pro';
import { GanttProps, Gantt } from '@choerodon/gantt';
import STATUS_COLOR from '@/constants/STATUS_COLOR';
import Context from '../../context';
import styles from './index.less';
import type { GanttIssue } from '../../types';

interface GanttBarProps {
  type: string
  bar: Gantt.Bar<GanttIssue>
  width: number
  height: number
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
  type, bar, width, height, onClick,
}) => {
  const { store } = useContext(Context);
  const { ganttRef } = store;
  const { record: issue, loading, stepGesture } = bar;
  const statusType = issue.statusVO.type;
  const subTasks = issue.children ? issue.children.filter((i) => i.issueTypeVO?.typeCode === 'sub_task') : [];
  const hasChildren = subTasks && subTasks.length > 0;
  const totalCount = subTasks?.length || 0;
  const completeCount = subTasks?.filter((item) => item.completed).length || 0;
  // @ts-ignore
  const [color1, color2] = STATUS_COLOR[statusType];
  const percent = totalCount ? completeCount / totalCount : 0;
  let diff = 0;
  let delayDiff = 0;
  if (issue.estimatedStartTime && issue.estimatedEndTime) {
    // 延期
    // if (dayjs(issue.estimatedEndTime).isBefore(dayjs()) && !issue.completed) {
    //   color1 = '#FF5C6A';
    //   color2 = '#FFBAC0';
    // }
    diff = dayjs(issue.estimatedEndTime).diff(issue.estimatedStartTime, 'hour');
  }
  const delayWidth = (() => {
    if (!issue.estimatedEndTime || loading) {
      return 0;
    }
    const actualCompletedDate: Dayjs = issue.completed && issue.actualCompletedDate ? dayjs(issue.actualCompletedDate).startOf('day') : dayjs().hour(0).minute(0).second(0);
    const endDate: Dayjs = dayjs(issue.estimatedEndTime).endOf('day');
    if (actualCompletedDate.isBefore(endDate) || actualCompletedDate.isSame(endDate)) {
      return 0;
    }
    delayDiff = (issue.actualCompletedDate ? dayjs(issue.actualCompletedDate) : dayjs()).diff(dayjs(issue.estimatedEndTime), 'hour');
    return (ganttRef.current?.getWidthByDate(endDate, actualCompletedDate) || 0) + (issue.completed && issue.actualCompletedDate ? 0 : 15);
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
          width,
          height,
          backgroundColor: color2,
          borderColor: color1,
          display: 'flex',
          borderRadius: delayVisible && delayWidth > 0 ? '2px 0 0 2px' : '2px',
          overflow: 'hidden',
        }}
        >
          <div style={{ flex: totalCount > 0 ? completeCount : 1, backgroundColor: color1 }} />
          <div style={{ flex: totalCount > 0 ? totalCount - completeCount : 0 }} />
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
            style={{ width: delayWidth, marginLeft: width }}
          />
        )}
      </Tooltip>
    </>
  );
};
export default observer(GanttBar);
