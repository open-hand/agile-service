import React, {
  useMemo, forwardRef, useRef, useContext,
} from 'react';
import { Select } from 'choerodon-ui/pro';
import { stores } from '@choerodon/boot';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import FormContext from 'choerodon-ui/pro/lib/form/FormContext';
import { FlatSelect } from '@choerodon/components';
import { useCreation, usePersistFn } from 'ahooks';
import classNames from 'classnames';
import { uniqBy, castArray, includes } from 'lodash';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { projectApi, ganttApi } from '@/api';
import { ICategoryCode } from '@/hooks/useCategoryCodes';
import { styles } from '../common/utils';
import projectStyles from './index.less';
// 用于查询组织下的项目

const { AppState } = stores;

export interface SelectTeamProps extends Omit<Partial<SelectProps>, 'optionRenderer'> {
  optionRenderer?: SelectConfig<any>['optionRenderer']
  projectDataRef?: React.RefObject<Array<any>>,
  afterLoad?: (projects: any[]) => void
  /** 只查询组织下敏捷项目 */
  queryAgile?: boolean
  extraOptions?: Array<{ id: string, name: string }>
  level?: 'workbench'
  defaultSelectedIds?: string[]
  flat?: boolean
  optionData?: any[]
  userId?: string,
  category?: ICategoryCode | ICategoryCode[],
  excludeIds?: [],
}

const SelectProject = forwardRef<Select | undefined, SelectTeamProps>(({
  userId, projectDataRef = { current: null }, afterLoad, defaultSelectedIds: propsDefaultSelectedIds, level, flat, extraOptions, category, optionData, queryAgile, popupCls, excludeIds, optionRenderer, ...otherProps
}, ref: React.Ref<Select>) => {
  const afterLoadRef = useRef<Function>();
  afterLoadRef.current = afterLoad;
  const defaultSelectedIds = useCreation(() => castArray(propsDefaultSelectedIds).filter(Boolean), []);
  const context = useContext(FormContext);
  const config = useMemo((): SelectConfig<any> => ({
    name: 'team',
    textField: 'name',
    valueField: 'id',
    tooltip: true,
    request: ({ filter, page, requestArgs }) => {
      if (optionData) {
        return optionData;
      }
      if (level === 'workbench') {
        return projectApi.loadProjectForWorkbench({
          page,
          size: 50,
          param: filter,
          userId: userId ?? AppState?.userInfo?.id,
          filterProjectIds: defaultSelectedIds,
        });
      }

      return queryAgile ? ganttApi.loadProjects() : projectApi.loadProjectByUser({
        userId: userId ?? AppState?.userInfo?.id,
        filter,
        page,
        size: 50,
        category: castArray(category || []).join(','),
      });
    },
    optionRenderer,
    // @ts-ignore
    afterLoad: afterLoadRef.current,
    middleWare: (projects) => {
      const newProjects = uniqBy([...(extraOptions || []), ...projects].map((item) => ({ ...item, id: String(item.id) })), 'id').filter((item) => !includes(excludeIds, item.id));
      // @ts-ignore
      // eslint-disable-next-line
      projectDataRef.current = newProjects;
      return newProjects || [];
    },
    paging: !queryAgile && !optionData,
  }), [category, defaultSelectedIds, excludeIds, extraOptions, level, optionData, optionRenderer, projectDataRef, queryAgile, userId]);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;
  return (
    <Component
      ref={ref}
      popupCls={classNames({ [styles.popup]: !context.formNode }, projectStyles.popup, popupCls)}
      {...props}
      {...otherProps}

    />
  );
});
export default SelectProject;
