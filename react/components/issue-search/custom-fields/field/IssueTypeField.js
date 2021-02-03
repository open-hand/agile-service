import React, { useMemo, useState } from 'react';
import { observer } from 'mobx-react-lite';
import { unionBy } from 'lodash';
import SelectFocusLoad from '@/components/SelectFocusLoad';
import { configTheme } from '@/utils/common';
import { issueTypeApi } from '@/api';
import SelectIssueType from '@/components/select/select-issue-type';
import { getSelectStyle } from '../utils';

function IssueTypeField({
  field, value, onChange, projectId, applyType,
}) {
  return (
    <SelectIssueType
      projectId={projectId}
      applyType={applyType}
      key={field.code}
      flat
      value={value || []}
      placeholder={field.name}
      multiple
      maxTagCount={3}
      dropdownMatchSelectWidth={false}
      clearButton
      onChange={onChange}
    />
  );
}
export default observer(IssueTypeField);
