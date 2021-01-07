import React, { useMemo, useState } from 'react';
import { observer } from 'mobx-react-lite';
import SelectFeature from '@/components/select/select-feature';
import { featureApi } from '@/api';

function FeatureField({ field, value, onChange }) {
  const defaultValue = useMemo(() => value, []);
  return (
    <SelectFeature
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
