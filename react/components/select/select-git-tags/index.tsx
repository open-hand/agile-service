import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { devOpsApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { ILabel } from '@/common/types';
import FlatSelect from '@/components/flat-select';

interface Props extends Partial<SelectProps> {
  dataRef?: React.RefObject<Array<any>>
  valueField?: string
  afterLoad?: (sprints: ILabel[]) => void
  applicationId?: string
  flat?: boolean
  projectId?: string
}

const SelectGitTags: React.FC<Props> = forwardRef(({
  dataRef, valueField, afterLoad, flat, applicationId, projectId, ...otherProps
}, ref: React.Ref<Select>) => {
  console.log('applicationId..', applicationId);
  const config = useMemo((): SelectConfig => ({
    name: 'tag',
    textField: 'name',
    valueField: 'name',
    request: ({ page }) => (applicationId ? devOpsApi.loadTagsByService(applicationId, page, 20, {}) : (() => new Promise([] as any))),
    middleWare: (data: any) => {
      if (dataRef) {
        Object.assign(dataRef, {
          current: data,
        });
      }
      if (afterLoad) {
        afterLoad(data);
      }
      return data;
    },
    paging: true,
  }), [applicationId]);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;

  return (
    <Component
      ref={ref}
      disabled={!applicationId}
      {...props}
      {...otherProps}
    />
  );
});
export default SelectGitTags;
