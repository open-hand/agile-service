import React, { useMemo } from 'react';
import { Tooltip } from 'choerodon-ui/pro';
import useProjectPredecessorTypes from '@/hooks/data/useProjectPredecessorTypes';
import styles from './index.less';

interface IGanttPredecessorProps {
  data: Array<{ predecessorType: string, [key: string]: any }>
  projectId?: string
}
const GanttPredecessor: React.FC<IGanttPredecessorProps> = ({ projectId, data }) => {
  const { data: predecessorTypes = [] } = useProjectPredecessorTypes({ projectId });
  const predecessorMaps = useMemo(() => new Map(predecessorTypes.map((item) => [item.valueCode, item])), [predecessorTypes]);
  const predecessors: any[] = useMemo(() => data?.map((item: any) => ({ ...item, predecessorName: predecessorMaps.get(item.predecessorType)?.name })) || [], [data, predecessorMaps]);

  return (
    <Tooltip
      placement="topLeft"
      title={(
        <div className={styles.overlay}>
          {predecessors.map((predecessor: any) => <div>{`${predecessor.predecessorName}：${predecessor.issueNum} ${predecessor.summary}`}</div>)}
        </div>
)}
    >
      <span>
        {predecessors.map<string>((predecessor: any) => `${predecessor.issueNum?.split('-').slice(-1)}${predecessor.predecessorName}`).join('、')}
      </span>
    </Tooltip>
  );
};
export default GanttPredecessor;
