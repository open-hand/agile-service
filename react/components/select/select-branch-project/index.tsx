import React, {
  useMemo, useCallback, forwardRef, useRef,
} from 'react';
import { Select } from 'choerodon-ui/pro';
import { stores } from '@choerodon/boot';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import { usePersistFn } from 'ahooks';
import classNames from 'classnames';
import { uniqBy } from 'lodash';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { projectApi, ganttApi } from '@/api';
import { ICategoryCode } from '@/hooks/useCategoryCodes';
import renderEllipsisBlockOption, { styles } from '../select-pi/utils';
import projectStyles from './index.less';
// 用于创建分支 关联分支的项目列表

const { AppState } = stores;

export interface SelectBranchProjectProps extends Omit<Partial<SelectProps>, 'optionRenderer'> {
  optionRenderer?: SelectConfig<any>['optionRenderer']
  flat?: string
  afterLoad?: SelectConfig<any>['afterLoad']
  extraOptions?: Array<{ id: string, name: string }>
  userId?: string
  currentProjectId?: string
}

const SelectBranchProject: React.FC<SelectBranchProjectProps> = forwardRef(({
  flat, popupCls, optionRenderer, afterLoad, currentProjectId, extraOptions, userId, ...otherProps
}, ref: React.Ref<Select>) => {
  const afterLoadRef = useRef<Function>();
  afterLoadRef.current = afterLoad;
  const renderProjectOption = useCallback(
    (optionData: any) => renderEllipsisBlockOption(optionData?.name, <>本项目</>, { blockClassName: projectStyles.current, showBlock: String(currentProjectId) === String(optionData.id) }),
    [currentProjectId],
  );
  const config = useMemo((): SelectConfig<any> => ({
    name: 'team',
    textField: 'name',
    valueField: 'id',
    tooltip: true,
    request: ({ filter, page }) => projectApi.loadFromBranch({
      currentProjectId, page, size: 50, param: filter, userId: userId ?? AppState?.userInfo?.id,
    }),
    optionRenderer: optionRenderer || renderProjectOption,
    // @ts-ignore
    afterLoad: afterLoadRef.current,
    middleWare: (projects) => {
      const newProjects = uniqBy([...(extraOptions || []), ...projects].map((item) => ({ ...item, id: String(item.id) })), 'id');
      return newProjects || [];
    },
    paging: true,
  }), [currentProjectId, extraOptions, optionRenderer, renderProjectOption, userId]);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;
  return (
    <Component
      ref={ref}
      popupCls={classNames(styles.popup, popupCls)}
      {...props}
      {...otherProps}

    />
  );
});
export default SelectBranchProject;
