/* eslint-disable no-underscore-dangle */
import React, { useContext } from 'react';
import { observer } from 'mobx-react-lite';
import dayjs, { Dayjs } from 'dayjs';
import minMax from 'dayjs/plugin/minMax';
import { Gantt } from '@choerodon/gantt';
import { Issue } from '@/common/types';
import Context from '../../stores/context';

dayjs.extend(minMax);
interface GanttGroupBarProps {
  bar: Gantt.Bar<Issue>
  width: number
  height: number
}
function getDelayRange(bar: Gantt.Bar<Issue>) {
  let maxEstimatedEndTime: Dayjs | undefined;
  let maxActualCompletedDate: Dayjs | undefined;
  const temp = [bar];

  while (temp.length > 0) {
    const current = temp.shift();
    if (current) {
      const { estimatedEndTime, actualCompletedDate } = current.record;
      if (estimatedEndTime) {
        if (!maxEstimatedEndTime) {
          maxEstimatedEndTime = dayjs(estimatedEndTime);
        } else {
          maxEstimatedEndTime = dayjs.max(dayjs(estimatedEndTime), maxEstimatedEndTime);
        }
      }
      if (actualCompletedDate) {
        if (!maxActualCompletedDate) {
          maxActualCompletedDate = dayjs(actualCompletedDate);
        } else {
          maxActualCompletedDate = dayjs.max(dayjs(actualCompletedDate), maxActualCompletedDate);
        }
      } else {
        maxActualCompletedDate = dayjs.max(dayjs(), maxActualCompletedDate || dayjs());
      }
      if (current.task.children && current.task.children.length > 0) {
        current.task.children.forEach((t) => {
          if (t._bar) {
            temp.push(t._bar);
          }
        });
      }
    }
  }

  return {
    maxEstimatedEndTime,
    maxActualCompletedDate,
  };
}
const GanttGroupBar: React.FC<GanttGroupBarProps> = ({
  bar, width: planWidth,
}) => {
  const { store } = useContext(Context);

  const { ganttRef } = store;
  const delayWidth = (() => {
    // eslint-disable-next-line prefer-const
    let { maxEstimatedEndTime, maxActualCompletedDate } = getDelayRange(bar);
    if (maxEstimatedEndTime) {
      if (!maxActualCompletedDate) {
        maxActualCompletedDate = dayjs();
      }
      const start = maxEstimatedEndTime.endOf('day');
      const end = maxActualCompletedDate.startOf('day');
      if (start.isBefore(end)) {
        return (ganttRef.current?.getWidthByDate(start, end) || 0) + 15;
      }
    }
    return 0;
  })();
  const height = 16;
  const pos = height / 2;
  const width = planWidth + delayWidth;
  const percent = (planWidth / width) * 100;
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      version="1.1"
      width={width + 1}
      height={height + 8}
      viewBox={`0 0 ${width + 1} ${height + 8}`}
    >
      <defs>
        <linearGradient id={String(bar.key)} x1="0%" y1="0%" x2="100%" y2="0%">
          <stop
            offset={`${percent}%`}
            style={{
              stopColor: '#7B809E',
              stopOpacity: 1,
            }}
          />
          <stop
            offset={`${percent}%`}
            style={{
              stopColor: '#FF5C6A',
              stopOpacity: 1,
            }}
          />
        </linearGradient>
      </defs>
      <polygon
        points={
          `
    0,0 
    ${width},0
    ${width},${height} 
    ${width - pos},${height - pos} 
    ${pos},${height - pos} 
    0,${height} 0,0
    `
        }
        fill={`url(#${String(bar.key)})`}
      />
    </svg>
  );
};
export default observer(GanttGroupBar);
