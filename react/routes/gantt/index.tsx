import React, { useState } from 'react';
import { getMenuType } from '@/utils/common';
import Gantt from './Gantt';
import useIsInProgram from '@/hooks/useIsInProgram';
import TableCache from '@/components/table-cache';

const GanttProject = () => {
  const { isInProgram, loading } = useIsInProgram();

  return !loading ? (
    <TableCache type="gantt">
      {(cacheProps) => <Gantt {...cacheProps} isInProgram={isInProgram} menuType="project" />}
    </TableCache>
  ) : null;
};
const GanttOrg = () => {
  const [type, setType] = useState('');
  return <Gantt cached={{} as any} updateCache={() => {}} isInProgram={false} menuType="org" />;
};
const GanttProjectOrg = () => {
  if (getMenuType() === 'project') {
    return <GanttProject />;
  }
  return <GanttOrg />;
};
export default GanttProjectOrg;
