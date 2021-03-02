import React, { ReactNode } from 'react';
import { toJS } from 'mobx';
import { observer } from 'mobx-react';
import omit from 'lodash/omit';
import autobind from 'choerodon-ui/pro/lib/_util/autobind';
// @ts-ignore
import WYSIWYGEditor from '@/components/WYSIWYGEditor';
import { FormField, FormFieldProps } from 'choerodon-ui/pro/lib/field/FormField';

interface TestProps extends FormFieldProps {
  placeholder?: string
}
@observer
class Editor<T extends TestProps> extends FormField<T> {
  getLabel() {
    return null;
  }

  @autobind
  handleChange(value: string) {
    this.setValue(value);
  }

  getOtherProps() {
    const otherProps = super.getOtherProps();
    return otherProps;
  }

  renderWrapper(): ReactNode {
    const otherProps = omit(this.getOtherProps(), ['onFocus', 'onBlur']);
    const { placeholder } = this.props;
    return (
      <div key="wrapper" {...this.getWrapperProps()}>
        <div>
          <WYSIWYGEditor
            {...otherProps}
            placeholder={placeholder}
            disabled={!this.editable}
            value={toJS(this.getValue())}
            style={{
              width: '100%',
              height: 200,
            }}
          />
          {this.renderFloatLabel()}
        </div>
      </div>
    );
  }
}

export default Editor;
