import React, {
  useEffect, useMemo, useRef,
} from 'react';
import { observer } from 'mobx-react-lite';
import SearchArea from './SearchArea';
import IssueSearchContext from './context';
import IssueSearchStore, { IssueSearchStoreProps } from './store';
import './index.less';
import useIsInProgram from '@/hooks/useIsInProgram';
import useIsProgram from '@/hooks/useIsProgram';

export interface IssueSearchProps {
  store: IssueSearchStore
  onClear: () => void
  onClickSaveFilter?: () => void
  urlFilter?: string
  onChange: () => void
  projectId?: string
  programId?: string
  applyType?: string
  foldedHeight?: number
  hasMyAssigned?: boolean
  excludeQuickFilterIds?: string[]
}
export function useIssueSearchStore(props: IssueSearchStoreProps) {
  const store = useMemo(() => new IssueSearchStore(props), []);
  return store;
}
export { IssueSearchStore };
const IssueSearch: React.FC<IssueSearchProps> = ({
  urlFilter, onClear, onClickSaveFilter, store, onChange, projectId, programId, applyType, foldedHeight = 43, hasMyAssigned = true, excludeQuickFilterIds = [],
}) => {
  const mountedRef = useRef<boolean>();
  const { isProgram } = useIsProgram();
  const { isInProgram, artInfo } = useIsInProgram(undefined, { enabled: !programId && store.menuType === 'program' });
  store.setQuery(onChange);
  useEffect(() => {
    store.initChosenFields();
  }, [store]);
  useEffect(() => {
    switch (store.menuType) {
      case 'program': {
        if (store.menuType === 'program' && (isProgram || isInProgram || programId)) {
          !isProgram && store.setProgramId(programId ?? (artInfo as any)?.programId);
          store.loadCustomFields();
        }
        break;
      }
      default: {
        if (!mountedRef.current) {
          store.loadMyFilterList();
          store.loadCustomFields();
          mountedRef.current = true;
        }
        break;
      }
    }
  }, [artInfo, isInProgram, isProgram, programId, store]);

  return (
    <IssueSearchContext.Provider
      value={{
        store,
        urlFilter,
        onClear,
        onClickSaveFilter,
        projectId,
        applyType,
        foldedHeight,
        hasMyAssigned,
        excludeQuickFilterIds,
      }}
    >
      <SearchArea />
    </IssueSearchContext.Provider>
  );
};
export default observer(IssueSearch);
