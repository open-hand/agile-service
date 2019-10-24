import React, { useEffect } from 'react';
import { Select } from 'choerodon-ui';

const { Option } = Select;

const DefaultOpenSelect = ({ ...otherProps }) => {
  const textInput = React.createRef();
  useEffect(() => {
    // 当select允许清空时，不自动下拉
    if (!textInput.current.props.allowClear) {
      textInput.current.rcSelect.onDropdownVisibleChange(true);
    }
  }, []);
  return (
    <Select ref={textInput} {...otherProps} />
  );
};

DefaultOpenSelect.Option = Option;
export default DefaultOpenSelect;
