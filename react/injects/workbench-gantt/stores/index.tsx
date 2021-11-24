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
import WorkbenchGantt from '../Gantt';
import useHeaderFullScreenButton, { IHeaderFullScreenButtonComponentProps } from '@/hooks/useHeaderButtons/useHeaderFullScreenButton';

const Context = createContext({ fullButtonProps: undefined } as { fullButtonProps?: IHeaderFullScreenButtonComponentProps });

export const StoreProvider = inject('AppState')(injectIntl(
  (props: any) => {
    const {
      children,
    } = props;
    const [, fullButtonProps] = useHeaderFullScreenButton({ appendCustomClassName: 'c7n-gantt-fullScreen' });

    return (
      <Context.Provider value={{ fullButtonProps }}>
        {children}
      </Context.Provider>
    );
  },
));
const Gantt: React.FC<IGanttProps> = (props) => {
  const { fullButtonProps } = useContext(Context);
  console.log('fullButtonProps', fullButtonProps);
  return (
    <GanntStoreProvider {...props} fullButtonProps={fullButtonProps}>
      <WorkbenchGantt />
    </GanntStoreProvider>
  );
};
export default Gantt;
