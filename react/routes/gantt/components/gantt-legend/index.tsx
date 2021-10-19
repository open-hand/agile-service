import React from 'react';
import SprintIcon from './SprintIcon';
import styles from './index.less';
import STATUS from '@/constants/STATUS';

function GanttLegendItem({ children }: any) {
  return (
    <div className={styles.item}>
      {children}
    </div>
  );
}
const colorBlockList = [
  { text: '待处理', background: STATUS.todo },
  { text: '处理中', background: STATUS.doing },
  { text: '已完成', background: STATUS.done },
  { text: '预计开始/结束', border: '1px dashed #5365EA' },
  { text: '实际开始/结束', background: '#5365EA' },
  { text: '延期', background: '#FF5C6A' },
  { text: '进度', background: 'linear-gradient(to right, #5365EA 50%, rgba(83,101,234,0.60) 50%)' }];
function GanttLegend() {
  return (
    <div
      className={styles.legend}
    >
      <GanttLegendItem>

        <SprintIcon />
        <span className={styles.label}>
          冲刺时间范围
        </span>
      </GanttLegendItem>
      <GanttLegendItem>
        <svg
          xmlns="http://www.w3.org/2000/svg"
          version="1.1"
          width={34}
          height={14}
        >
          <defs>
            <pattern
              id="repeat"
              width="2.5"
              height="10"
              patternUnits="userSpaceOnUse"
              patternTransform="rotate(70 50 50)"
            >
              <line stroke="#D9E6F2" strokeWidth="1px" y2="10" />
            </pattern>
          </defs>
          <g stroke="#D9E6F2">
            <rect
              fill="url(#repeat)"
              strokeWidth="1"
              x={1}
              y={1}
              rx={2}
              ry={2}
              width={32}
              height={12}
            />
          </g>
        </svg>
        <span
          className={styles.label}
        >
          节假日
        </span>
      </GanttLegendItem>
      {colorBlockList.map((block) => (
        <GanttLegendItem>
          <div className={styles.block} style={{ background: block.background, border: block.border }} />
          <span className={styles.label}>
            {block.text}
          </span>
        </GanttLegendItem>
      ))}
    </div>
  );
}
export default GanttLegend;
