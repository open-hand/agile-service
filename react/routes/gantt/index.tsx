import React, { useState, useEffect, useRef } from 'react';
import {
  Page, Header, Content, Breadcrumb, HeaderButtons,
} from '@choerodon/boot';
import { EmptyPage, FlatSelect } from '@choerodon/components';
import { find } from 'lodash';
import {
  Tooltip, Icon, Button, CheckBox, Select,
} from 'choerodon-ui/pro';
import TableCache from '@/components/table-cache';
import useIsInProgram from '@/hooks/useIsInProgram';
import { getMenuType, getProjectId } from '@/utils/common';
import { ganttApi } from '@/api';
import noDataPic from '@/assets/image/NoData.svg';
import Gantt from './Gantt';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';

const { Option } = Select;
// eslint-disable-next-line react/require-default-props
const GanttProject: React.FC<{ projectId: string, menuType?: 'project' | 'org', HeadSelect?: JSX.Element, [key: string]: any }> = ({
  projectId, projects, setCurrentProject, menuType = 'project',
}) => {
  const { isInProgram, loading } = useIsInProgram();

  if (menuType === 'project') {
    return !loading ? (
      <TableCache type="gantt" projectId={projectId}>
        {(cacheProps) => <Gantt {...cacheProps} isInProgram={isInProgram} menuType="project" projectId={projectId} />}
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
  const [loading, setLoading] = useState(true);
  useEffect(() => {
    ganttApi.loadProjects().then((res: any) => {
      const cacheProjectId = localPageCacheStore.getItem('org.gantt.projectId');
      const newProjects = res.map((i: any) => ({ ...i, id: String(i.id) }));
      const newProjectId = (find(newProjects, { id: cacheProjectId }) || newProjects[0])?.id;
      localPageCacheStore.setItem('org.gantt.projectId', newProjectId);
      setProjectIds(newProjects);
      setCurrentProject(newProjectId);
      setLoading(false);
    });
  }, []);
  if (!loading && projectIds.length === 0) {
    return (
      <Page>
        <Header>
          <FlatSelect placeholder="项目" />
        </Header>
        <Breadcrumb />
        <Content>
          <EmptyPage image={noDataPic} description="组织下无敏捷项目，请您先创建敏捷项目" />
        </Content>
      </Page>
    );
  }
  return <GanttProject projectId={currentProject} setCurrentProject={setCurrentProject} projects={projectIds} menuType="org" projectIds={projectIds} />;
};
const GanttProjectOrg = () => {
  console.log('......');
  const [value, setValue] = useState('4');
  const [option, setOption] = useState<any[]>([]);
  const ref = useRef<any>();
  useEffect(() => {
    setTimeout(() => {
      console.log('ref', ref);
      // ref.current?.checkValue();
    }, 1000);
  }, []);
  return (
    <div style={{ margin: 40 }}>
      22
      <Button onClick={() => setOption((old) => [...old, old.length])}>ADD</Button>
      <FlatSelect
        // value={value}
        valueChangeCheckValue
        defaultValue={['4']}
        multiple
        checkValueOnOptionsChange
        onChange={(v) => { console.log('onChange', v); }}
        ref={ref}
      >
        <Option value="1">选项</Option>
        <Option value="2">2选项</Option>
        <Option value="3">3选项</Option>
        {option.map((i) => <Option value={i}>{`${i}选项`}</Option>)}
      </FlatSelect>
    </div>
  );
};
const GanttProjectOrg0 = () => {
  if (getMenuType() === 'project') {
    return (
      <GanttProject projectId={getProjectId()} />
    );
  }
  return <GanttOrg />;
};
export default GanttProjectOrg0;
