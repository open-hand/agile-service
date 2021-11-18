import React, {
  useState, useEffect,
} from 'react';
import {
  Page, Header, Content, Breadcrumb, HeaderButtons,
} from '@choerodon/boot';
import { observer } from 'mobx-react-lite';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import SelectSprint from '@/components/select/select-sprint';
import { useGanttContext } from './stores/context';
import StatusLinkageWSHandle from '@/components/StatusLinkageWSHandle';
import SelectProject from '@/components/select/select-project';
import useGanttHeader from './hooks/useGanttHeader';
import SelectType from './components/gannt-select/SelectType';
import SelectProcessType from './components/gannt-select/SelectProcessType';
import GanttBody from './GanttBody';
import './index.less';

const GanttPage: React.FC = () => {
  const {
    menuType, projectId, projects, setCurrentProject, store, issueSearchStore,
  } = useGanttContext();
  const [ganttHeaderData, {
    typeComponentProps, sprintComponentsProps, processTypeComponentProps, headerComponentProps,
  }] = useGanttHeader({
    projectId,
    store,
  });
  // const [loading, setLoading] = useState(false);

  return (
    <Page>
      <Header>
        {menuType === 'org' && (
          <SelectProject
            value={projectId}
            flat
            clearButton={false}
            style={{ marginRight: 16 }}
            maxTagTextLength={12}
            optionData={projects}
            onChange={(val) => {
              setCurrentProject && setCurrentProject((oldValue: string) => {
                if (oldValue === val || !val) {
                  return String(oldValue);
                }
                store.setSprintIds(null);
                localPageCacheStore.setItem('org.gantt.projectId', val);
                return val;
              });
            }}
          />
        )}
        <SelectSprint {...sprintComponentsProps} />
        <SelectType {...typeComponentProps} />
        <SelectProcessType {...processTypeComponentProps} />
        <HeaderButtons {...headerComponentProps} />
      </Header>
      <Breadcrumb />
      <Content
        className="c7n-gantt-content"
        style={{
          borderTop: '1px solid var(--divider)',
          display: 'flex',
          paddingTop: 7,
          flexDirection: 'column',
          paddingBottom: 0,
        }}
      >
        <GanttBody {...ganttHeaderData} />
      </Content>
      <StatusLinkageWSHandle />
    </Page>
  );
};

export default observer(GanttPage);
