import React, { useMemo, forwardRef, useRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { commonApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import FlatSelect from '@/components/flat-select';

interface Props extends Partial<SelectProps> {
  projectDataRef?: React.RefObject<Array<any>>,
  afterLoad?: (projects: any) => void
  noAssign?: boolean
  flat?: boolean
  request?: () => Promise<any>
  textField?: string
  valueField?: string
  projectId?: string
}

const SelectTeam: React.FC<Props> = forwardRef(({
  request, textField, valueField, projectDataRef = { current: null }, afterLoad, flat, projectId, noAssign, ...otherProps
}, ref: React.Ref<Select>) => {
  const afterLoadRef = useRef<Function>();
  afterLoadRef.current = afterLoad;
  const config = useMemo((): SelectConfig => ({
    name: 'team',
    textField: textField || 'projName',
    valueField: valueField || 'projectId',
    request: () => {
      if (!request) {
        return commonApi.getSubProjects(true, projectId);
      }
      return request();
    },
    paging: false,
    // @ts-ignore
    afterLoad: afterLoadRef.current,
    middleWare: (projects) => {
      if (Array.isArray(projects)) {
        // @ts-ignore
        // eslint-disable-next-line
        projectDataRef.current = projects;
        return projects || [];
      }
      // @ts-ignore

      // eslint-disable-next-line no-param-reassign
      projectDataRef.current = (projects as any).content || [];
      const list = (projects as any).content || [];
      return noAssign ? [{ projName: '未分配', projectId: '0' }, ...list] : list;
    },
  }), [noAssign]);
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
export default SelectTeam;
