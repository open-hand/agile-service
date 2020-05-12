import React, { useMemo, useEffect, useState } from 'react';
import { Select } from 'choerodon-ui/pro';
import { debounce } from 'lodash';
import { userApi } from '@/api';

function SelectUser({ onChange, ...props }) {
  const [users, setUsers] = useState([]);
  async function loadUserData(value) {
    const { list: data } = await userApi.getAllInProject(value);
    setUsers(data);
  }

  const searchData = useMemo(() => debounce((value) => {
    loadUserData(value);
  }, 500), []);
  useEffect(() => {
    searchData('');
  }, []);
  return (
    <Select
      style={{ width: '100%' }}
      searchable
      searchMatcher="param"
      placeholder="人员"
      maxTagCount={4}
      maxTagTextLength={12}
      maxTagPlaceholder={restValues => `+${restValues.length}...`}
      onInput={(e) => { searchData(e.target.value); }}
      {...props}
    >
      {users.map(user => <Select.Option key={user.id} value={user.id}>{user.realName}</Select.Option>)}
    </Select>
  );
}
export default SelectUser;
