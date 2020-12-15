import React, { useContext, useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import dayjs, { Dayjs } from 'dayjs';
import { Tooltip } from 'choerodon-ui/pro';
import { GanttProps, Gantt } from 'react-gantt-component';
import STATUS_COLOR from '@/constants/STATUS_COLOR';
import Context from '../../context';
import styles from './index.less';

interface GanttBarProps {
  type: string
  bar: Gantt.Bar
  width: number
  height: number
  onClick:GanttProps['onBarClick']
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
  const { task: issue, loading, stepGesture } = bar;
  const statusType = issue.statusVO.type;
  const subTasks = issue.children ? issue.children.filter((i) => i.issueTypeVO.typeCode === 'sub_task') : [];
  const hasChildren = subTasks && subTasks.length > 0;
  const totalCount = subTasks?.length || 0;
  const completeCount = subTasks?.filter((item) => item.completed).length || 0;
  // @ts-ignore
  const [color1, color2] = STATUS_COLOR[statusType];
  const percent = totalCount ? completeCount / totalCount : 0;
  let diff = 0;
  if (issue.estimatedStartTime && issue.estimatedEndTime) {
    // 延期
    // if (dayjs(issue.estimatedEndTime).isBefore(dayjs()) && !issue.completed) {
    //   color1 = '#FF5C6A';
    //   color2 = '#FFBAC0';
    // }
    diff = dayjs(issue.estimatedEndTime).diff(issue.estimatedStartTime, 'hour');
  }
  const delayWidth = useMemo(() => {
    if (!issue.endDate || loading) {
      return 0;
    }
    const actualCompletedDate: Dayjs = issue.actualCompletedDate ? dayjs(issue.actualCompletedDate).endOf('day') : dayjs().hour(0).minute(0).second(0);
    const endDate: Dayjs = dayjs(issue.endDate).endOf('day');
    if (actualCompletedDate.isBefore(endDate) || actualCompletedDate.isSame(endDate)) {
      return 0;
    }
    return (ganttRef.current?.getWidthByDate(endDate, actualCompletedDate) || 0) + (issue.actualCompletedDate ? 0 : 15);
  }, [loading, ganttRef, issue.actualCompletedDate, issue.endDate]);
  const delayVisible = stepGesture !== 'moving' && !loading;
  return (
    <>
      <Tooltip title={(
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
      </Tooltip>
      {delayVisible && (
        <div
          role="none"
          onMouseDown={(e) => {
            e.stopPropagation();
          }}
          onClick={(e) => {
            e.stopPropagation();
            onClick && onClick(bar.task);
          }}
          className={styles.delay}
          style={{ width: delayWidth, marginLeft: width }}
        />
      )}
    </>
  );
};
export default observer(GanttBar);
