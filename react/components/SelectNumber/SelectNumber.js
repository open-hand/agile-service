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
    const { loose } = this.props;
    // 宽松模式，可以输入小数点，外部校验格式
    if (loose) {
      if ((/^[0-9.]+$/.test(value) || value === '')) {
        this.triggerChange(value);
      } else {
        this.triggerChange(preValue);
      }
    } else {
      // 只允许输入整数，选择时可选0.5
      // eslint-disable-next-line no-lonely-if
      if (value === '0.5') {
        this.triggerChange('0.5');
      } else if (/^(0|[1-9][0-9]*)(\[0-9]*)?$/.test(value) || value === '') {
        this.triggerChange(String(value).slice(0, 3));
      } else if (value.toString().charAt(value.length - 1) === '.') {
        this.triggerChange(value.slice(0, -1));
      } else {
        this.triggerChange(preValue);
      }
    }
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
