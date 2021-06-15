import React, {
  useMemo, forwardRef, useEffect, useRef,
} from 'react';
import { Select, Tooltip } from 'choerodon-ui/pro';
import { Observer, useComputed, useForceUpdate } from 'mobx-react-lite';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { devOpsApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { ILabel } from '@/common/types';
import { FlatSelect } from '@choerodon/components';
import { omit, pick } from 'lodash';

interface Props extends Partial<SelectProps> {
  dataRef?: React.RefObject<Array<any>>
  ref?: any
  valueField?: string
  afterLoad?: (sprints: any[]) => void
  applicationId?: string | null
  checkMember?:boolean
  flat?: boolean
  projectId?: string

}

const SelectGitTags: React.FC<Props> = forwardRef(({
  dataRef, valueField, afterLoad, flat, applicationId, projectId, checkMember, ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig => ({
    name: 'tag',
    textField: 'name',
    valueField: 'name',
    request: ({ page, filter }) => (applicationId ? devOpsApi.project(projectId).loadTagsByService(applicationId, page, 20, {
      searchParam: {
        tagName: filter,
      },
      param: '',
    }, checkMember) : (() => new Promise([] as any))),
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
  }), [applicationId, projectId]);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;

  if (!applicationId) {
    return (
      <Tooltip title="请先选择应用服务">
        <div {...pick(otherProps, ['style', 'className'])}>
          <Component ref={ref} disabled {...omit(otherProps, ['style', 'className'])} style={{ width: '100%' }} />
        </div>
      </Tooltip>
    );
  }
  return (
    <Component
      ref={ref}
      {...props}
      {...otherProps}
    />
  );
});
const SelectGitTagsHOC: React.FC<Props> = forwardRef(({
  applicationId, projectId, ...otherProps
}, ref: React.Ref<Select>) => {
  const innerRef = useRef<Select>(null);
  const forceUpdate = useForceUpdate();

  useEffect(() => {
    forceUpdate();
  }, [applicationId, projectId]);
  function handleBindRef(r: any) {
    Object.assign(innerRef, { current: r });
    typeof (ref) === 'function' ? ref(r) : ref && Object.assign(ref, { current: r });
  }

  return <SelectGitTags ref={handleBindRef} applicationId={applicationId} projectId={projectId} {...otherProps} key={`select-git-tag-${applicationId}-${projectId}`} />;
});
export default SelectGitTagsHOC;
