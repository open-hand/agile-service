import React, { useMemo, useRef, useState } from 'react';
import { observer } from 'mobx-react-lite';
import SelectFeature from '@/components/select/select-feature';
import { featureApi } from '@/api';

function FeatureField({
  field, value, onChange, projectId,
}) {
  const ref = useRef();
  const [key, updateKey] = useState(0);
  useMemo(() => {
    if (ref.current) {
      // 有新的未加载的值，就重新设置key
      const hasNewUnExistValue = value.some((v) => !ref.current.options.find((record) => record.get('issueId') === v));
      if (hasNewUnExistValue) {
        updateKey((k) => k + 1);
      }
    }
  }, [value]);
  return (
    <SelectFeature
      useSelectRef={ref}
      projectId={projectId}
      key={`${field.code}${key}`}
      flat
      value={value || []}
      placeholder={field.name}
      multiple
      maxTagCount={3}
      maxTagTextLength={10}
      request={({ filter, page }) => featureApi.queryAllInSubProject(value, filter, page)}
      dropdownMatchSelectWidth={false}
      clearButton
      onChange={onChange}
    />
  );
}
export default observer(FeatureField);
