import React, { Component } from 'react';
import {
  Select,
} from 'choerodon-ui';

const selectValues = ['0.5', '1', '2', '3', '4', '5', '8', '13'];
const { Option } = Select;
class SelectNumber extends Component {
  constructor(props) {
    super(props);
    this.state = {
      value: props.value || undefined,
    };
  }

  static getDerivedStateFromProps(nextProps) {
    if ('value' in nextProps) {
      return {
        value: nextProps.value,
      };
    }
    return null;
  }

  handleChange = (value) => {
    const { value: preValue } = this.state;

    this.triggerChange(String(value));
  };

  triggerChange = (value) => {
    const { onChange } = this.props;
    if (!('value' in this.props)) {
      this.setState({ value });
    }
    if (onChange) {
      onChange(value);
    }
  }

  render() {
    const { selectNumbers } = this.props;
    const { value } = this.state;
    const options = selectNumbers || selectValues;
    return (
      <Select
        getPopupContainer={(triggerNode) => triggerNode.parentNode}
        // autoFocus
        {...this.props}
        value={value}
        mode="combobox"
        tokenSeparators={[',']}
        onChange={this.handleChange}
      >
        {options.map((sp) => (
          <Option key={sp.toString()} value={sp}>
            {sp}
          </Option>
        ))}
      </Select>
    );
  }
}
export default SelectNumber;
