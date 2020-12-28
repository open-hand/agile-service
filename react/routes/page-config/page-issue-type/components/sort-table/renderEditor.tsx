import { Select } from 'choerodon-ui/pro/lib';
import React, { forwardRef } from 'react';

const DatePickerPage = forwardRef((props, ref) => (
  <Select
    ref={ref as any}
    popupContent={
      (selectProps) => (
        <div>
          <span>当前时间</span>
          <span>选择日期</span>
        </div>
      )
    }
  />
));
function renderEditor(props?:any) {
  const bn = 0;
  console.log('renderEditor', props);
  return <DatePickerPage {...props} />;
}
export default renderEditor;
