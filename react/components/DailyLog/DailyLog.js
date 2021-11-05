/* eslint-disable react/jsx-no-bind */
import React, { Component } from 'react';
import { stores, Choerodon } from '@choerodon/boot';
import moment from 'moment';
import {
  Select, DatePicker, Modal, Radio,
} from 'choerodon-ui';
import { Button, Tooltip } from 'choerodon-ui/pro';
import { workLogApi } from '@/api';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import SelectNumber from '@/components/select/select-number';
import WYSIWYGEditor from '../CKEditor';
import './DailyLog.less';

const DATA_FORMAT = 'YYYY-MM-DD HH:mm:ss';
const { Sidebar } = Modal;
const { Option } = Select;
const { AppState } = stores;
const RadioGroup = Radio.Group;
const TYPE = {
  1: 'self_adjustment',
  2: 'no_set_prediction_time',
  3: 'set_to',
  4: 'reduce',
};
const storyPointList = ['0.5', '1', '2', '3', '4', '5', '8', '13'];

class DailyLog extends Component {
  constructor(props) {
    super(props);
    this.state = {
      dissipate: undefined,
      dissipateUnit: '小时',
      startTime: moment(),
      radio: 1,
      time: undefined,
      timeUnit: '小时',
      reduce: undefined,
      reduceUnit: '小时',
      delta: '',
      createLoading: false,
      dissipateNull: false,
      startTimeNull: false,
      loading: false,
      uploading: false,
    };
    this.selectRef = React.createRef();
  }

  componentDidMount() {
    // setTimeout(() => {
    //   this.Select.focus();
    // });
  }

  onRadioChange = (e) => {
    this.setState({ radio: e.target.value });
  }

  handleFullEdit = (delta) => {
    this.setState({
      delta,
      edit: false,
    });
  }

  handleCreateDailyLog = async () => {
    const {
      dissipate, startTime, radio, delta, uploading,
    } = this.state;
    const { issueId } = this.props;
    if ((!await this.selectRef.current.checkValidity())) {
      return;
    }
    if (uploading) {
      Choerodon.prompt('请等待图片上传完成');
      return;
    }
    if (!startTime) {
      this.setState({
        dissipateNull: !dissipate,
        startTimeNull: !startTime,
      });
      return;
    }
    this.setState({ createLoading: true });
    let num;
    if (radio === '3' || radio === 3) {
      num = this.transformTime('time', 'timeUnit');
    }
    if (radio === '4' || radio === 4) {
      num = this.transformTime('reduce', 'reduceUnit');
    }
    try {
      const text = delta;
      const extra = {
        issueId,
        projectId: AppState.currentMenuType.id,
        startDate: startTime.format('YYYY-MM-DD HH:mm:ss'),
        workTime: this.transformTime('dissipate', 'dissipateUnit'),
        residualPrediction: TYPE[radio],
        predictionTime: [3, 4].indexOf(radio) === -1 ? undefined : num,
        description: text,
      };
      this.handleSave(extra);
    } catch (error) {
      this.setState({ createLoading: false });
    }
  };

  handleSave = (data) => {
    const { onOk } = this.props;
    this.setState({
      loading: true,
    });
    workLogApi.create(data)
      .then((res) => {
        this.setState({
          loading: false,
        });
        onOk();
      }).catch(() => {
        this.setState({
          loading: false,
        });
      });
  };

  handleDissipateChange = (e) => {
    this.setState({ dissipate: e || '' });
  }

  handleDissipateUnitChange = (value) => {
    this.setState({ dissipateUnit: value });
  }

  handleTimeChange = (e) => {
    this.setState({ time: e || '' });
  }

  handleTimeUnitChange = (value) => {
    this.setState({ timeUnit: value });
  }

  handleReduceChange = (e) => {
    this.setState({ reduce: e || '' });
  }

  handleReduceUnitChange = (value) => {
    this.setState({ reduceUnit: value });
  }

  changeEndTime = (value) => {
    const startTime = this.transTime(value);
    this.setState({ startTime: value, startTimeNull: !value });
  }

  handleChangeDissipate = (value) => {
    this.setState({
      dissipate: value,
    });
  };

  handleChangeTime = (value) => {
    const { time } = this.state;
    // 只允许输入整数，选择时可选0.5
    if (value === '0.5') {
      this.setState({
        time: '0.5',
      });
    } else if (/^(0|[1-9][0-9]*)(\[0-9]*)?$/.test(value) || value === '') {
      this.setState({
        time: String(value).slice(0, 3), // 限制最长三位,
      });
    } else if (value.toString().charAt(value.length - 1) === '.') {
      this.setState({
        time: value.slice(0, -1),
      });
    } else {
      this.setState({
        time,
      });
    }
  };

  handleChangeReduce = (value) => {
    const { reduce } = this.state;
    // 只允许输入整数，选择时可选0.5
    if (value === '0.5') {
      this.setState({
        reduce: '0.5',
      });
    } else if (/^(0|[1-9][0-9]*)(\[0-9]*)?$/.test(value) || value === '') {
      this.setState({
        reduce: String(value).slice(0, 3), // 限制最长三位,
      });
    } else if (value.toString().charAt(value.length - 1) === '.') {
      this.setState({
        reduce: value.slice(0, -1),
      });
    } else {
      this.setState({
        reduce,
      });
    }
  };

  isEmpty(data) {
    return data === '' || data === undefined || data === null;
  }

  transformTime(pro, unit) {
    const { state } = this;
    const TIME = new Map([
      ['小时', 1],
      ['天', 8],
      ['周', 40],
    ]);
    if (!state[pro]) {
      return 0;
    }
    return state[pro] * TIME.get(state[unit]);
  }

  formDate(data) {
    const temp = data ? new Date(data) : new Date();
    return `${temp.getFullYear()}-${temp.getMonth() + 1}-${temp.getDate()}`;
  }

  transTime(data) {
    if (this.isEmpty(data)) {
      return undefined;
    } if (typeof data === 'string') {
      return moment(this.formDate(data), DATA_FORMAT);
    }
    return data.format('YYYY-MM-DD HH:mm:ss');
  }

  render() {
    const {
      initValue, visible, onCancel, onOk, issueNum,
    } = this.props;
    const {
      createLoading, dissipate, dissipateUnit,
      startTime, radio, time, timeUnit, reduce,
      reduceUnit, delta, edit, startTimeNull, loading, dissipateNull, uploading,
    } = this.state;
    const radioStyle = {
      display: 'block',
      width: '100%',
      height: '30px',
      lineHeight: '30px',
      marginBottom: '15px',
    };

    return (
      <Sidebar
        maskClosable
        className="c7n-dailyLog"
        title="登记工作日志"
        visible={visible || false}
        cancelText="取消"
        footer={[
          <Button key="back" onClick={onCancel} funcType="raised">取消</Button>,
          <Button key="submit" color="primary" funcType="raised" loading={loading} disabled={loading} onClick={this.handleCreateDailyLog}>
            确定
          </Button>,
        ]}
        width={MODAL_WIDTH.middle}
      >
        <div>
          <section className="info">
            <div className="line-info" style={{ alignItems: 'flex-start' }}>
              <div style={{ flex: 1, marginTop: 0, paddingTop: 0 }}>
                <SelectNumber
                  ref={this.selectRef}
                  required
                  style={{ width: '100%' }}
                  labelLayout="float"
                  value={dissipate}
                  label="耗费时间"
                  onChange={(value) => this.handleChangeDissipate(value)}
                />
              </div>
              <Select
                value={dissipateUnit}
                style={{ width: 160, marginLeft: 18 }}
                onChange={this.handleDissipateUnitChange.bind(this)}
              >
                {['小时', '天', '周'].map((type) => (
                  <Option key={type} value={type}>{type}</Option>))}
              </Select>
            </div>
            <div
              className="dataPicker"
              style={{
                width: '100%', margin: '32px 0', display: 'flex', flexDirection: 'column', position: 'relative',
              }}
            >
              <DatePicker
                style={{ width: '100%' }}
                label={(
                  <span>
                    工作日期
                    <span style={{ color: 'red' }}>*</span>
                  </span>
                )}
                placeholder={(
                  <span>
                    工作日期
                    <span style={{ color: 'red' }}>*</span>
                  </span>
                )}
                value={startTime}
                format={DATA_FORMAT}
                onChange={this.changeEndTime}
              />
              {startTimeNull
                ? <div className="error-text">工作日期必填</div>
                : ''}
            </div>
            <div className="line-info">
              <RadioGroup label="剩余的估计" onChange={this.onRadioChange} value={radio}>
                <Radio style={radioStyle} value={1}>自动调整</Radio>
                <Radio style={radioStyle} value={2}>不自动调整剩余预估时间</Radio>
                <Radio
                  style={{
                    ...radioStyle,
                    marginBottom: 20,
                  }}
                  value={3}
                >
                  <span style={{ display: 'inline-block', width: 52 }}>设置为</span>
                  <Select
                    disabled={radio !== 3}
                    value={time && time.toString()}
                    mode="combobox"
                    ref={(e) => {
                      this.componentRef = e;
                    }}
                    onPopupFocus={(e) => {
                      this.componentRef.rcSelect.focus();
                    }}
                    tokenSeparators={[',']}
                    style={{ width: 265, marginTop: 0, paddingTop: 0 }}
                    onChange={(value) => this.handleChangeTime(value)}
                  >
                    {storyPointList.map((sp) => (
                      <Option key={sp.toString()} value={sp}>
                        {sp}
                      </Option>
                    ))}
                  </Select>
                  <Select
                    disabled={radio !== 3}
                    style={{ width: 160, marginLeft: 18 }}
                    value={timeUnit}
                    onChange={this.handleTimeUnitChange.bind(this)}
                  >
                    {['小时', '天', '周'].map((type) => (
                      <Option key={`${type}`} value={`${type}`}>{type}</Option>))}
                  </Select>
                </Radio>
                <Radio
                  style={{
                    ...radioStyle,
                    marginBottom: 20,
                  }}
                  value={4}
                >
                  <span style={{ display: 'inline-block', width: 52 }}>缩减</span>
                  <Select
                    disabled={radio !== 4}
                    value={reduce && reduce.toString()}
                    mode="combobox"
                    ref={(e) => {
                      this.componentRef = e;
                    }}
                    onPopupFocus={(e) => {
                      this.componentRef.rcSelect.focus();
                    }}
                    tokenSeparators={[',']}
                    style={{ width: 265, marginTop: 0, paddingTop: 0 }}
                    onChange={(value) => this.handleChangeReduce(value)}
                  >
                    {storyPointList.map((sp) => (
                      <Option key={sp.toString()} value={sp}>
                        {sp}
                      </Option>
                    ))}
                  </Select>
                  <Select
                    disabled={radio !== 4}
                    style={{ width: 160, marginLeft: 18 }}
                    value={reduceUnit}
                    onChange={this.handleReduceUnitChange.bind(this)}
                  >
                    {['小时', '天', '周'].map((type) => (
                      <Option key={`${type}`} value={`${type}`}>{type}</Option>))}
                  </Select>
                </Radio>
              </RadioGroup>
            </div>

            <div className="c7n-sidebar-info">
              {
                !edit && (
                  <div className="clear-p-mw">
                    <WYSIWYGEditor
                      value={delta}
                      style={{ width: '100%', minHeight: 300 }}
                      onChange={(value) => {
                        this.setState({ delta: value });
                      }}
                      onUploadChange={(v) => {
                        this.setState({
                          uploading: v,
                        });
                      }}
                    />
                  </div>
                )
              }
            </div>
          </section>
        </div>
      </Sidebar>
    );
  }
}
export default DailyLog;
