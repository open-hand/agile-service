import React, {
  useMemo, useContext, createContext,
} from 'react';
import { inject } from 'mobx-react';
import { injectIntl } from 'react-intl';
import { useIssueSearchStore } from '@/components/issue-search';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import { transformFilter } from '@/routes/Issue/stores/utils';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import { ILocalField } from '@/components/issue-search/store';
import { IGanttProps } from '@/routes/gantt/stores/context';
import { StoreProvider as GanntStoreProvider } from '@/routes/gantt/stores';

const Context = createContext({});
export const StoreProvider = inject('AppState')(injectIntl(
  (props: any) => {
    const {
      children,
    } = props;
    return (
      <Context.Provider value={{}}>
        {children}
      </Context.Provider>
    );
  },
));
const Gantt: React.FC<IGanttProps> = (props) => (
  <GanntStoreProvider {...props}>
    <div />
  </GanntStoreProvider>
);
export default Gantt;
