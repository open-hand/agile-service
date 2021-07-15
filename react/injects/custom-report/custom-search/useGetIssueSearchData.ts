import ChoseFieldStore from '@/components/chose-field/store';
import { IssueSearchStore } from '@/components/issue-search';
import useQuickFilters from '@/components/issue-search/useQuickFilters';
import { flattenObject, isFilterSame } from '@/components/issue-search/utils';
import { stores } from '@choerodon/boot';
import { useDebounce, usePersistFn } from 'ahooks';
import { find, noop } from 'lodash';
import { toJS } from 'mobx';
import { useCallback, useMemo } from 'react';

interface IGetIssueSearchDataConfigProps {
  store: IssueSearchStore
  chosenStore: ChoseFieldStore
  onChoseField?: (key: string) => void
  projectId?: string
}
interface IGetIssueSearchDataProps {
  onChange: (key: string, value?: any, option?: any) => void | boolean,
  onClear: () => void
}
const { AppState } = stores;

function useGetIssueSearchData(config: IGetIssueSearchDataConfigProps): IGetIssueSearchDataProps {
  const {
    store, projectId, onChoseField: propsOnChoseField, chosenStore,
  } = config;
  const onChoseField = usePersistFn(propsOnChoseField || noop);
  const {
    isHasFilter, chosenFields, overflowLine, folded,
  } = store;
  const userId = String(AppState.userInfo.id);
  const { data: quickFilters } = useQuickFilters({ projectId });
  const myFilters = store.getMyFilters;
  const selectedQuickFilterIds = chosenFields.get('quickFilterIds') ? toJS(chosenFields.get('quickFilterIds')?.value) : undefined;
  const hasSummaryField = useMemo(() => store.getAllFields.some((f) => f.code === 'contents'), [store.getAllFields]);

  const getSelectedQuickFilters = () => (selectedQuickFilterIds || []).map((id: string) => {
    const target = find(quickFilters, { filterId: id });
    return target ? `quick|${target.filterId}` : undefined;
  });
  const selectedQuickFilters: string[] = getSelectedQuickFilters();
  const findSameFilter = useCallback(() => {
    const currentFilterDTO = store.getCustomFieldFilters()
      ? flattenObject(store.getCustomFieldFilters()) : {};
    // 找到与当前筛选相同条件的我的筛选
    const targetMyFilter = find(myFilters,
      (filter) => isFilterSame(flattenObject(JSON.parse(filter.filterJson)), currentFilterDTO));
    return targetMyFilter;
  }, [myFilters, store]);
  const getMyFilterSelectValue = () => {
    const targetMyFilter = findSameFilter();

    const currentFilterDTO = store.getCustomFieldFilters();
    const onlyMe = currentFilterDTO.otherArgs.assigneeId && currentFilterDTO.otherArgs.assigneeId.length === 1 && currentFilterDTO.otherArgs.assigneeId[0] === userId;
    const { starBeacon, myAssigned } = currentFilterDTO.otherArgs;
    const result = [...selectedQuickFilters];
    if (targetMyFilter) {
      result.push(`my|${targetMyFilter.filterId}`);
    }
    if (onlyMe) {
      result.push('commonly|onlyMe');
    }
    if (starBeacon) {
      result.push('commonly|starBeacon');
    }
    if (myAssigned) {
      result.push('commonly|myAssigned');
    }
    return result;
  };
  const myFilterSelectValue = getMyFilterSelectValue();

  const reset = useCallback(() => {
    store.clearAllFilter();
    store.query();
  }, [store]);
  const handleFilterChange = useCallback((code: string, value: any) => {
    store.handleFilterChange(code, value);
    onChoseField(code, value);
  }, [onChoseField, store]);
  const handleInputChange = useCallback((value: string) => {
    if (value) {
      handleFilterChange('contents', [value]);
    } else {
      handleFilterChange('contents', []);
    }
    handleFilterChange('issueIds', []);
  }, [handleFilterChange]);
  const handleClearFilter = useCallback((v?: string) => {
    if (!hasSummaryField) {
      reset();
      return;
    }
    const content: string[] = store.getFilterValueByCode('contents') || [];
    const sameFilter = findSameFilter();
    const isSelectedOption = sameFilter && (!v || v === sameFilter.filterId);
    if (content[0] && content[0] !== '' && !isSelectedOption) {
      store.clearAllFilter();
      handleInputChange(content[0]);
    } else {
      reset();
    }
  }, [findSameFilter, handleInputChange, hasSummaryField, reset, store]);
  /**
   *   快速筛选 使用
   */
  const handleSelect = useCallback((k: string) => {
    const [type, id] = k.split('|');
    if (type === 'quick') {
      const newSelectedQuickFilters = [...selectedQuickFilters, k];
      const quickFilterIds = newSelectedQuickFilters.map((filter) => filter.split('|')[1]);
      handleFilterChange('quickFilterIds', quickFilterIds);
    } else if (type === 'my') {
      const targetMyFilter = find(myFilters, { filterId: id });
      const filterObject = flattenObject(JSON.parse(targetMyFilter?.filterJson || '{}'));
      // 先清除筛选
      store.clearAllFilter();
      for (const [key, value] of Object.entries(filterObject)) {
        // 自定义字段保存的时候只保存了id，这里要找到code
        // @ts-ignore
        if (value && isObject(value) && value.isCustom) {
          const code = store.getFieldCodeById(key);
          if (code) {
            // @ts-ignore
            handleFilterChange(code, value.value);
          }
        } else if (key === 'createEndDate' || key === 'createStartDate') {
          handleFilterChange('createDate', [filterObject.createStartDate, filterObject.createEndDate]);
        } else if (key === 'updateEndDate' || key === 'updateStartDate') {
          handleFilterChange('updateDate', [filterObject.updateStartDate, filterObject.updateEndDate]);
        } else {
          handleFilterChange(key, value);
        }
      }
      if (folded) {
        store.setFolded(false);
      }
    } else if (type === 'commonly') {
      if (id === 'onlyMe') {
        handleFilterChange('assigneeId', [userId]);
      } else if (id === 'starBeacon') {
        handleFilterChange('starBeacon', true);
        handleFilterChange('userId', userId);
      } else if (id === 'myAssigned') {
        handleFilterChange('myAssigned', true);
        handleFilterChange('userId', userId);
      }
    }
  }, [folded, handleFilterChange, myFilters, selectedQuickFilters, store, userId]);
  /**
   * 快速筛选 使用
   */
  const handleDeselect = useCallback((key: string) => {
    const [type, id] = key.split('|');
    if (type === 'quick') {
      const quickFilterIds = selectedQuickFilters.filter((k) => k !== key).map((filter) => filter.split('|')[1]);
      handleFilterChange('quickFilterIds', quickFilterIds);
    } else if (type === 'my') {
      handleClearFilter(id);
    } else if (type === 'commonly') {
      if (id === 'onlyMe') {
        handleFilterChange('assigneeId', []);
      } else if (id === 'starBeacon') {
        handleFilterChange('starBeacon', undefined);
        handleFilterChange('userId', undefined);
      } else if (id === 'myAssigned') {
        handleFilterChange('myAssigned', undefined);
        handleFilterChange('userId', undefined);
      }
    }
  }, [handleClearFilter, handleFilterChange, selectedQuickFilters]);

  const handlePersonalFilterChange = useCallback((values: string[] | null) => {
    if (!values) {
      handleClearFilter();
      return;
    }
    // 取消选择
    if (myFilterSelectValue.length > (values as string[]).length) {
      const unSelected = find(myFilterSelectValue, (v) => !values?.includes(v as string));
      if (unSelected) {
        handleDeselect(unSelected);
      }
      // 选择
    } else {
      const newSelect = find(values, (v) => !myFilterSelectValue?.includes(v));
      if (newSelect) {
        handleSelect(newSelect);
      }
    }
  }, [handleClearFilter, handleDeselect, handleSelect, myFilterSelectValue]);
  const handleChange = useCallback((key: string, v: any) => {
    handleFilterChange(key, v);
  }, [handleFilterChange]);
  const onChange = useCallback((key: string, value: any, option?: any) => {
    if (key === 'quickFilterIds') {
      handlePersonalFilterChange(value);
      return;
    }
    handleChange(key, value);
  }, [handleChange, handlePersonalFilterChange]);
  return {
    onChange,
    onClear: handleClearFilter,
  };
}
export default useGetIssueSearchData;
