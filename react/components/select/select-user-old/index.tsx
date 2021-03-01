import React, { forwardRef, useMemo, useState } from 'react';
import { Select } from 'choerodon-ui';
import useSelect from '@/hooks/useSelectOld';
import useProjectUsers from '@/hooks/data/useProjectUsers';
import { SelectProps } from 'choerodon-ui/lib/select';
import UserHead from '@/components/UserHead';
import { User } from '@/common/types';

const { Option } = Select;
export interface SelectUserProps extends Partial<SelectProps> {
  extraOption?: User | User[],
  projectId?: string
}

const SelectUser: React.FC<SelectUserProps> = forwardRef(({
  extraOption, projectId, ...otherProps
}, ref: React.Ref<Select>) => {
  const [text, setText] = useState('');
  const {
    fetchNextPage, data, hasNextPage, isLoading,
  } = useProjectUsers({ param: text });
  const options = useMemo(() => {
    if (!extraOption) {
      return data;
    }
    return Array.isArray(extraOption) ? [...extraOption.filter((o) => !!o), ...data] : [extraOption, ...data];
  }, [data, extraOption]);
  const [props] = useSelect({
    data: options,
    render: (user) => (
      <Option value={user.id}>
        <div style={{
          display: 'inline-flex', alignItems: 'center', padding: 2, verticalAlign: 'sub',
        }}
        >
          <UserHead
            user={user}
          />
        </div>
      </Option>
    ),
    hasNextPage,
    fetchNextPage,
    onSearch: setText,
    paging: true,
  });

  return (
    <Select
      ref={ref}
      loading={isLoading}
      {...props}
      {...otherProps}
    />
  );
});
SelectUser.displayName = 'SelectUser';
export default SelectUser;
