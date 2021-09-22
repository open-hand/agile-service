import React from 'react';
import { omit } from 'lodash';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { ILabel } from '@/common/types';
import SelectMultiServiceTag from '../select-multi-service-tag';

const prefixCls = 'c7n-agile-select-multi-service-tag';
interface Props extends Partial<SelectProps> {
  dataRef?: React.RefObject<Array<any>>
  valueField?: string
  afterLoad?: (sprints: ILabel[]) => void
  applicationId?: string | null
  flat?: boolean
  projectId?: string

}
class SelectMultiServiceTagOld extends React.PureComponent<Props, any> {
  constructor(props: Props) {
    super(props);
    this.state = { showValue: this.props.value, value: this.props.value };
  }

  get getOtherProps() {
    return omit(this.props, 'onBlur', 'trigger', 'onChange');
  }

  get getValue() {
    return this.state.value;
  }

  get getShowText() {
    return this.state.showValue;
  }

  handleChange = (value: any) => {
    const { onChange } = this.props;
    let newValue = value;
    if (!value || (Array.isArray(value) && typeof (value[0]) === 'string')) {
      this.setState({ showValue: newValue, value });
    } else if ((Array.isArray(value))) {
      newValue = value.map((i) => `${i.appServiceCode}:${i.tagName}`);
      this.setState({ showValue: newValue, value });
    }

    if (onChange) {
      onChange(value, '');
    }
  };

  render() {
    return <SelectMultiServiceTag {...this.getOtherProps} multiple value={this.getValue} onChange={this.handleChange} />;
  }
}
export default SelectMultiServiceTagOld;
