import React, {
  useMemo, forwardRef, useEffect, useRef, useCallback, useState,
} from 'react';
import {
  Select, DataSet, Form, Progress, Button,
} from 'choerodon-ui/pro';
import { omit } from 'lodash';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { observer, useForceUpdate } from 'mobx-react-lite';
import { toJS } from 'mobx';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { devOpsApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { ILabel } from '@/common/types';
import { FlatSelect } from '@choerodon/components';
import { getProjectId } from '@/utils/common';
import SelectAppService from '../select-app-service';
import SelectGitTags from '../select-git-tags';
import styles from './index.less';
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
    console.log('value....SelectMultiServiceTagOld', value);
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
