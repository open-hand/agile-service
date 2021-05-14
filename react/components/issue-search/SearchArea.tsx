import React, { useContext, useMemo, useCallback } from 'react';
import {
  Icon, Tooltip,
} from 'choerodon-ui';
import { stores } from '@choerodon/boot';
import { find, remove } from 'lodash';
import { toJS } from 'mobx';
import { observer } from 'mobx-react-lite';
import { Button } from 'choerodon-ui/pro';
import { FuncType, ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import { FlatSelect } from '@choerodon/components';
import { flattenObject, isFilterSame } from './utils';
import IssueSearchContext from './context';
import SummaryField from './custom-fields/field/SummaryField';
import CustomFields from './custom-fields';
import useQuickFilters from './useQuickFilters';
import ListenSearchSize from './ListenSearchSize';

const { AppState } = stores;
const { Option, OptGroup } = FlatSelect;

const SearchArea: React.FC = () => {
  const prefixCls = 'c7n-issue';
  const {
    store, onClear, urlFilter, onClickSaveFilter, projectId, foldedHeight,
  } = useContext(IssueSearchContext);
  const { data: quickFilters } = useQuickFilters({ projectId });
  const {
    isHasFilter, chosenFields, overflowLine, folded,
  } = store;
  const myFilters = store.getMyFilters;
  const selectedQuickFilterIds = chosenFields.get('quickFilterIds') ? toJS(chosenFields.get('quickFilterIds')?.value) : undefined;
  const archiveFields = [...chosenFields.values()].filter((field) => field.archive);
  const getSelectedQuickFilters = () => (selectedQuickFilterIds || []).map((id: string) => {
    const target = find(quickFilters, { filterId: id });
    return target ? `quick|${target.filterId}` : undefined;
  });
  const selectedQuickFilters: string[] = getSelectedQuickFilters();
  const userId = String(AppState.userInfo.id);
  const reset = () => {
    onClear();
    store.clearAllFilter();
    store.query();
  };

  const handleSelect = (k: string) => {
    const [type, id] = k.split('|');
    if (type === 'quick') {
      const newSelectedQuickFilters = [...selectedQuickFilters, k];
      const quickFilterIds = newSelectedQuickFilters.map((filter) => filter.split('|')[1]);
      store.handleFilterChange('quickFilterIds', quickFilterIds);
    } else if (type === 'my') {
      const targetMyFilter = find(myFilters, { filterId: id });
      const filterObject = flattenObject(JSON.parse(targetMyFilter?.filterJson || '{}'));
      // 先清除筛选
      store.clearAllFilter();
      for (const [key, value] of Object.entries(filterObject)) {
        // 自定义字段保存的时候只保存了id，这里要找到code
        if (value.isCustom) {
          const code = store.getFieldCodeById(key);
          if (code) {
            store.handleFilterChange(code, value.value);
          }
        } else if (key === 'createEndDate' || key === 'createStartDate') {
          store.handleFilterChange('createDate', [filterObject.createStartDate, filterObject.createEndDate]);
        } else if (key === 'updateEndDate' || key === 'updateStartDate') {
          store.handleFilterChange('updateDate', [filterObject.updateStartDate, filterObject.updateEndDate]);
        } else {
          store.handleFilterChange(key, value);
        }
      }
      if (folded) {
        store.setFolded(false);
      }
    } else if (type === 'commonly') {
      if (id === 'onlyMe') {
        store.handleFilterChange('assigneeId', [userId]);
      } else if (id === 'starBeacon') {
        store.handleFilterChange('starBeacon', true);
        store.handleFilterChange('userId', userId);
      }
    }
  };
  const handleDeselect = (key: string) => {
    const [type, id] = key.split('|');
    if (type === 'quick') {
      const quickFilterIds = selectedQuickFilters.filter((k) => k !== key).map((filter) => filter.split('|')[1]);
      store.handleFilterChange('quickFilterIds', quickFilterIds);
    } else if (type === 'my') {
      handleClearFilter(id);
    } else if (type === 'commonly') {
      if (id === 'onlyMe') {
        store.handleFilterChange('assigneeId', []);
      } else if (id === 'starBeacon') {
        store.handleFilterChange('starBeacon', undefined);
        store.handleFilterChange('userId', undefined);
      }
    }
  };
  const handleClearFilter = (v?: string) => {
    if (!hasSummaryField) {
      reset();
      return;
    }
    const content: string[] = store.getFilterValueByCode('contents') || [];
    const sameFilter = findSameFilter();
    const isSelectedOption = sameFilter && (!v || v === sameFilter.filterId);
    if (content[0] && content[0] !== '' && !isSelectedOption) {
      onClear();
      store.clearAllFilter();
      handleInputChange(content[0]);
    } else {
      reset();
    }
  };
  const handlePersonalFilterChange = (values: string[] | null) => {
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
  };

  const findSameFilter = () => {
    const currentFilterDTO = store.getCustomFieldFilters()
      ? flattenObject(store.getCustomFieldFilters()) : {};
    // console.log(currentFilterDTO);
    // 找到与当前筛选相同条件的我的筛选
    const targetMyFilter = find(myFilters,
      (filter) => isFilterSame(flattenObject(JSON.parse(filter.filterJson)), currentFilterDTO));
    return targetMyFilter;
  };
  const getMyFilterSelectValue = () => {
    const targetMyFilter = findSameFilter();

    const currentFilterDTO = store.getCustomFieldFilters();
    const onlyMe = currentFilterDTO.otherArgs.assigneeId && currentFilterDTO.otherArgs.assigneeId.length === 1 && currentFilterDTO.otherArgs.assigneeId[0] === userId;
    const { starBeacon } = currentFilterDTO.otherArgs;
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
    return result;
  };
  const handleInputChange = (value: string) => {
    if (value) {
      store.handleFilterChange('contents', [value]);
    } else {
      store.handleFilterChange('contents', []);
    }
    store.handleFilterChange('issueIds', []);
  };
  const handleClickExpandFilter = useCallback(() => {
    store.setFolded(!folded);
  }, [folded, store]);

  const myFilterSelectValue = getMyFilterSelectValue();
  const hasSummaryField = useMemo(() => store.getAllFields.some((f) => f.code === 'contents'), [store.getAllFields]);
  const hasQuickFilterField = useMemo(() => store.getAllFields.some((f) => f.code === 'quickFilterIds'), [store.getAllFields]);
  const renderSearch = () => (
    <>
      {hasSummaryField && (
        <div style={{ marginTop: 4 }}>
          <SummaryField
            onChange={handleInputChange}
            value={store.getFilterValueByCode('contents') ? store.getFilterValueByCode('contents')[0] : undefined}
          />
        </div>
      )}
      <div className={`${prefixCls}-search-left`}>
        <CustomFields>
          {hasQuickFilterField ? (
            <div style={{ margin: '4px 5px' }}>
              <FlatSelect
                placeholder="我的筛选"
                value={myFilterSelectValue}
                onChange={handlePersonalFilterChange}
                popupCls={`${prefixCls}-search-hidden_my_filter`}
                multiple
              >
                <OptGroup key="commonly" label="常用选项">
                  <Option value="commonly|onlyMe">仅我的问题</Option>
                  <Option value="commonly|starBeacon">我的关注</Option>
                </OptGroup>
                <OptGroup key="quick" label="快速筛选">
                  {quickFilters.map((filter) => (
                    <Option value={`quick|${filter.filterId}`}>{filter.name}</Option>
                  ))}
                </OptGroup>
                <OptGroup key="my" label="个人筛选">
                  {
                    myFilters.map((filter) => (
                      <Option value={`my|${filter.filterId}`}>{filter.name}</Option>
                    ))
                  }
                </OptGroup>
              </FlatSelect>
            </div>
          ) : null}
        </CustomFields>
      </div>
      <div className={`${prefixCls}-search-right`}>
        {isHasFilter && !folded && (
          <Button
            onClick={reset}
            funcType={'flat' as FuncType}
            className={`${prefixCls}-search-right-btn`}
          >
            重置
          </Button>
        )}
        {onClickSaveFilter && !findSameFilter() && isHasFilter && !folded && (
          <Tooltip title={archiveFields.length > 0 && `${archiveFields.map((f) => f.name).join(',')}字段已被废弃，请去掉该字段后保存`}>
            <Button
              onClick={archiveFields.length > 0 ? undefined : onClickSaveFilter}
              funcType={'flat' as FuncType}
              className={`${prefixCls}-search-right-btn ${prefixCls}-search-right-saveBtn`}
            >
              保存筛选
            </Button>
          </Tooltip>
        )}
        {
          (overflowLine || folded === true) && (
            <Tooltip title={folded ? '展开筛选' : '折叠筛选'}>
              <Button
                onClick={handleClickExpandFilter}
                funcType={'flat' as FuncType}
                className={`${prefixCls}-search-right-btn ${folded === true ? 'foldedBtn' : ''}`}
              >
                <Icon type={folded ? 'expand_more' : 'expand_less'} />
              </Button>
            </Tooltip>
          )
        }
      </div>
    </>
  );
  const renderUrlFilter = () => (
    <>
      <div className={`${prefixCls}-search-left`}>
        <div className={`${prefixCls}-search-urlFilter`}>
          <Icon type="search" />
          <div className={`${prefixCls}-search-urlFilter-item`}>{urlFilter}</div>
        </div>
      </div>
      <div className={`${prefixCls}-search-right`}>
        <Button style={{ marginTop: 10 }} onClick={reset} funcType={'flat' as FuncType} color={'blue' as ButtonColor}>重置</Button>
      </div>
    </>
  );
  return (
    <div
      className={`${prefixCls}-search`}
      style={{
        height: folded ? foldedHeight : 'unset',
        overflow: 'hidden',
      }}
    >
      <ListenSearchSize />
      {urlFilter ? renderUrlFilter() : renderSearch()}
    </div>
  );
};
export default observer(SearchArea);
