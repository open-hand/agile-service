import React from 'react';
import { SelectBox, Select, DatePicker } from 'choerodon-ui/pro';
import moment, { Moment } from 'moment';
import { LabelLayout } from 'choerodon-ui/pro/lib/form/enum';
import { ViewMode } from 'choerodon-ui/pro/lib/radio/enum';
import SelectService from './SelectService';

const { Option } = SelectBox;
export type ServiceCodeQualityType = 'issue' | 'coverage' | 'duplicate'
export interface ServiceCodeQualitySearchProps {
  projectId: string
  serviceId: string
  setServiceId: (serviceId: string) => void
  days: number | null
  setDays: (days: number) => void
  range?: [Moment, Moment] | null
  onRangeChange: (value: [Moment, Moment]) => void
  type: ServiceCodeQualityType
  setType: (type: ServiceCodeQualityType) => void
  onEmpty: () => void
}
const ServiceCodeQualityVarySearch: React.FC<ServiceCodeQualitySearchProps> = ({
  projectId,
  days,
  setDays,
  range,
  onRangeChange,
  serviceId,
  setServiceId,
  type,
  setType,
  onEmpty,
}) => (
  <div>
    <SelectService
      label="选择应用服务"
      labelLayout={'float' as LabelLayout}
      projectId={projectId}
      value={serviceId}
      onChange={setServiceId}
      afterLoad={(data) => {
        if (!serviceId) {
          if (data.length > 0) {
            setServiceId(data[0].id);
          } else {
            onEmpty();
          }
        }
      }}
      clearButton={false}
    />
    <Select
      label="对象类型"
      style={{ marginLeft: 24 }}
      value={type}
      onChange={setType}
      labelLayout={'float' as LabelLayout}
      clearButton={false}
    >
      <Option value="issue">
        问题
      </Option>
      <Option value="coverage">
        覆盖率
      </Option>
      <Option value="duplicate">
        重复度
      </Option>
    </Select>
    <SelectBox
      mode={'button' as ViewMode}
      style={{ marginLeft: 24 }}
      value={days}
      onChange={setDays}
      labelLayout={'float' as LabelLayout}
      clearButton={false}
    >
      <Option value={0}>
        今天
      </Option>
      <Option value={7}>
        近7天
      </Option>
      <Option value={30}>
        近30天
      </Option>
    </SelectBox>
    <DatePicker
      style={{ marginLeft: 24 }}
      range
      placeholder={['开始时间', '结束时间']}
      label=" "
      value={range}
      onChange={onRangeChange}
      clearButton={false}
      labelLayout={'float' as LabelLayout}
      max={moment()}
    />
  </div>
);
export default ServiceCodeQualityVarySearch;
