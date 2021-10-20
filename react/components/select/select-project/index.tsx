import React, { useMemo, forwardRef, useRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { stores } from '@choerodon/boot';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import { usePersistFn } from 'ahooks';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { projectApi, ganttApi } from '@/api';
import { ICategoryCode } from '@/hooks/useCategoryCodes';

// 用于查询组织下的项目

const { AppState } = stores;

export interface SelectTeamProps extends Partial<SelectProps> {
  projectDataRef?: React.RefObject<Array<any>>,
  afterLoad?: (projects: any[]) => void
  /** 只查询组织下敏捷项目 */
  queryAgile?: boolean
  flat?: boolean
  optionData?: any[]
  userId?: string,
  category?: ICategoryCode,
}

const SelectProject: React.FC<SelectTeamProps> = forwardRef(({
  userId, projectDataRef = { current: null }, afterLoad, flat, category, optionData, queryAgile, ...otherProps
}, ref: React.Ref<Select>) => {
  const afterLoadRef = useRef<Function>();
  afterLoadRef.current = afterLoad;
  const fakePageRequest = usePersistFn((filter: string = '') => {
    if (!optionData) {
      return [];
    }
    return optionData;
  });
  const config = useMemo((): SelectConfig => ({
    name: 'team',
    textField: 'name',
    valueField: 'id',
    request: ({ filter, page }) => {
      if (optionData) {
        return optionData;
      }
      return queryAgile ? ganttApi.loadProjects() : projectApi.loadProjectByUser({
        userId: userId ?? AppState?.userInfo?.id,
        filter,
        page,
        size: 50,
        category,
      });
    },
    // @ts-ignore
    afterLoad: afterLoadRef.current,
    middleWare: (projects) => {
      // @ts-ignore
      // eslint-disable-next-line
      projectDataRef.current = projects;
      return projects || [];
    },
    paging: !queryAgile && !optionData,
  }), [optionData, projectDataRef, queryAgile, userId]);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;
  return (
    <Component
      ref={ref}
      {...props}
      {...otherProps}
    />
  );
});
export default SelectProject;
