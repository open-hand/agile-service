import React, { useMemo, forwardRef, useRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { stores } from '@choerodon/boot';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { projectApi } from '@/api';

// 用于查询组织下的项目

const { AppState } = stores;

export interface SelectTeamProps extends Partial<SelectProps> {
  projectDataRef?: React.RefObject<Array<any>>,
  afterLoad?: (projects: any[]) => void
  flat?: boolean
  userId?: string,
}

const SelectProject: React.FC<SelectTeamProps> = forwardRef(({
  userId, projectDataRef = { current: null }, afterLoad, flat, ...otherProps
}, ref: React.Ref<Select>) => {
  const afterLoadRef = useRef<Function>();
  afterLoadRef.current = afterLoad;
  const config = useMemo((): SelectConfig => ({
    name: 'team',
    textField: 'name',
    valueField: 'id',
    request: ({ filter, page }) => projectApi.loadProjectByUser({
      userId: userId ?? AppState?.userInfo?.id,
      filter,
      page,
      size: 50,
    }),
    // @ts-ignore
    afterLoad: afterLoadRef.current,
    middleWare: (projects) => {
      // @ts-ignore
      // eslint-disable-next-line
        projectDataRef.current = projects;
      return projects || [];
    },
    paging: true,
  }), []);
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
