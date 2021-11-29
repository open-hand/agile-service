import React, {
  useMemo, useContext,
} from 'react';
import { inject } from 'mobx-react';
import Context, { IGanttProps } from './context';
import GanttStore from './store';
import { useIssueSearchStore } from '@/components/issue-search';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import { transformFilter } from '@/routes/Issue/stores/utils';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import { ILocalField } from '@/components/issue-search/store';
import GanttPage from '../Gantt';

export const StoreProvider = inject('AppState')(
  (props: IGanttProps & { children?: any }) => {
    const {
      children, projectId, myDefaultFilter, menuType,
    } = props;
    const store = useMemo(() => new GanttStore({ projectId }), [projectId]);
    const issueSearchStore = useIssueSearchStore({
      projectId,
      fieldConfigs: { issueTypeId: { excludeTypeCodes: ['issue_epic'] } },
      getSystemFields: () => getSystemFields().map((item) => (item.code === 'feature' || item.code === 'epic' ? { ...item, defaultShow: false } : item)).filter((item) => item.code !== 'sprint') as ILocalField[],
      transformFilter,
      defaultSearchVO: localPageCacheStore.project(projectId).getItem('agile.gantt.search') ?? (myDefaultFilter && myDefaultFilter.filterJson ? JSON.parse(myDefaultFilter.filterJson) : undefined) ?? undefined,
    });

    return (
      <Context.Provider value={{
        ...props, store, issueSearchStore, disable: menuType === 'org',
      }}
      >
        {children}
      </Context.Provider>
    );
  },
);
const Gantt: React.FC<IGanttProps> = (props) => (
  <StoreProvider {...props}>
    <GanttPage />
  </StoreProvider>
);
export default Gantt;
