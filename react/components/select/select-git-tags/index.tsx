import React, { useMemo, forwardRef, useEffect } from 'react';
import { Select } from 'choerodon-ui/pro';
import { useForceUpdate } from 'mobx-react-lite';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { devOpsApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { ILabel } from '@/common/types';
import FlatSelect from '@/components/flat-select';

interface Props extends Partial<SelectProps> {
  dataRef?: React.RefObject<Array<any>>
  valueField?: string
  afterLoad?: (sprints: ILabel[]) => void
  applicationId?: string | null
  flat?: boolean
  projectId?: string

}

const SelectGitTags: React.FC<Props> = forwardRef(({
  dataRef, valueField, afterLoad, flat, applicationId, projectId, ...otherProps
}, ref: React.Ref<Select>) => {
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
  useEffect(() => {
    console.log('Component useEffect into');
    return () => console.log('leave Component');
  }, []);
  const Component = flat ? FlatSelect : Select;
  return (
    <Component
      ref={ref}
      help={!applicationId ? '请先选择应用服务' : undefined}
      {...props}
      {...otherProps}
    />
  );
});
const SelectGitTagsHOC: React.FC<Props> = forwardRef(({
  applicationId, ...otherProps
}, ref: React.Ref<Select>) => {
  console.log('ref.....applicationId', applicationId);
  const forceUpdate = useForceUpdate();
  useEffect(() => {
    console.log('...', applicationId);
    forceUpdate();
  }, []);
  const component = (
    <SelectGitTags
    // @ts-ignore
      ref={ref}
      key={`select-git-tag-${applicationId}`}
      {...otherProps}
    />
  );
  return React.cloneElement(component, { key: `select-git-tag-${applicationId}` });
});
export default SelectGitTags;
