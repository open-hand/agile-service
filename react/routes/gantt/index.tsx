import React, { useState, useEffect } from 'react';

import '@choerodon/gantt/dist/gantt.cjs.production.min.css';
import TableCache from '@/components/table-cache';
import useIsInProgram from '@/hooks/useIsInProgram';
import Gantt from './Gantt';
import { getMenuType, getProjectId } from '@/utils/common';
import { ganttApi } from '@/api';

// eslint-disable-next-line react/require-default-props
const GanttProject: React.FC<{ projectId: string, menuType?: 'project' | 'org', HeadSelect?: JSX.Element, [key: string]: any }> = ({
  projectId, projects, setCurrentProject, menuType = 'project',
}) => {
  const { isInProgram, loading } = useIsInProgram();

  if (menuType === 'project') {
    return !loading ? (
      <TableCache type="gantt" projectId={projectId}>
        {(cacheProps) => <Gantt {...cacheProps} isInProgram={isInProgram} menuType="project" />}
      </TableCache>
    ) : <span />;
  }
  return projects.length && projectId && !loading ? (
    <TableCache type="gantt" projectId={projectId}>
      {(cacheProps) => (
        <Gantt {...cacheProps} isInProgram={isInProgram} menuType="org" projectId={projectId} projects={projects} setCurrentProject={setCurrentProject} />
      )}
    </TableCache>
  ) : null;
};
const GanttOrg = () => {
  const [currentProject, setCurrentProject] = useState<any>();
  const [projectIds, setProjectIds] = useState<any[]>([]);
  useEffect(() => {
    ganttApi.loadProjects().then((res: any) => {
      setProjectIds(res);
      setCurrentProject(res[0]?.id?.toString());
    });
  }, []);
  return <GanttProject projectId={currentProject} setCurrentProject={setCurrentProject} projects={projectIds} menuType="org" projectIds={projectIds} />;
};
const GanttProjectOrg = () => {
  if (getMenuType() === 'project') {
    return (
      <GanttProject projectId={getProjectId()} />
    );
  }
  return <GanttOrg />;
};
export default GanttProjectOrg;
