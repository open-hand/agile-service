import React, { useMemo, useState } from 'react';
import { observer } from 'mobx-react-lite';
import { unionBy } from 'lodash';
import SelectFocusLoad from '@/components/SelectFocusLoad';
import { configTheme } from '@/utils/common';
import SelectFeature from '@/components/select/select-feature';
import { featureApi } from '@/api';
import { getSelectStyle } from '../utils';

function FeatureField({
  field, value, onChange, projectId,
}) {
  const defaultValue = useMemo(() => value, []);
  return (
    <SelectFeature
      projectId={projectId}
      key={field.code}
      flat
      value={value || []}
      placeholder={field.name}
      multiple
      maxTagCount={3}
      maxTagTextLength={10}
      request={({ filter, page }) => featureApi.queryAllInSubProject(defaultValue, filter, page)}
      dropdownMatchSelectWidth={false}
      clearButton
      onChange={onChange}
    />
  );
}
export default observer(FeatureField);
