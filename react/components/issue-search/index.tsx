import React, {
  useEffect, useMemo, useRef,
} from 'react';
import { observer } from 'mobx-react-lite';
import { uniq } from 'lodash';
import SearchArea from './SearchArea';
import IssueSearchContext from './context';
import IssueSearchStore, { IssueSearchStoreProps } from './store';
import './index.less';
import useIsInProgram from '@/hooks/useIsInProgram';
import useIsProgram from '@/hooks/useIsProgram';

export type IIssueSearchCommonFilterOption = 'onlyMe' | 'starBeacon' | 'myAssignee'
export interface IssueSearchProps {
  store: IssueSearchStore
  onClear: () => void
  onClickSaveFilter?: () => void
  urlFilter?: string
  onChange: () => void
  projectId?: string
  programId?: string
  /** @default 'agile'  */
  applyType?: 'agile' | 'program' | 'risk' | ''
  foldedHeight?: number
  /**
   * 是否有我的筛选-通用选项 【我经办的】
   * @deprecated 使用 hiddenMyCommonFilterOption
   */
  hasMyAssigned?: boolean
  /**
   * 隐藏我的筛选通用选项
   */
  hiddenMyCommonFilterOption?: IIssueSearchCommonFilterOption[]
  excludeQuickFilterIds?: string[]
  hiddenQuickFilters?: boolean, // 隐藏快速筛选
  hiddenCustomFields?: boolean, // 隐藏自定义字段
}
export function useIssueSearchStore(props: IssueSearchStoreProps) {
  const store = useMemo(() => new IssueSearchStore(props), []);
  return store;
}
export { IssueSearchStore };
const IssueSearch: React.FC<IssueSearchProps> = ({
  urlFilter, onClear, onClickSaveFilter, store, onChange, projectId, programId, applyType = 'agile', foldedHeight = 43,
  hasMyAssigned: propsHasMyAssigned, excludeQuickFilterIds = [], hiddenQuickFilters = false, hiddenCustomFields = false,
  hiddenMyCommonFilterOption: propsHiddenMyCommonFilterOption,
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
          !hiddenQuickFilters && store.loadMyFilterList();
          !hiddenCustomFields && store.loadCustomFields();
          mountedRef.current = true;
        }
        break;
      }
    }
  }, [artInfo, isInProgram, isProgram, programId, store]);
  const hiddenMyCommonFilterOption = useMemo(() => {
    const hiddenOptions = [...(propsHiddenMyCommonFilterOption || []), propsHasMyAssigned ? 'myAssignee' : false].filter(Boolean);
    return uniq(hiddenOptions) as IIssueSearchCommonFilterOption[];
  }, [propsHasMyAssigned, propsHiddenMyCommonFilterOption]);
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
        hiddenMyCommonFilterOption,
        excludeQuickFilterIds,
        hiddenQuickFilters,
        hiddenCustomFields,
      }}
    >
      <SearchArea />
    </IssueSearchContext.Provider>
  );
};
export default observer(IssueSearch);
