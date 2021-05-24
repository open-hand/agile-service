import React from 'react';
import { SelectBox, Select, DatePicker } from 'choerodon-ui/pro';
import moment, { Moment } from 'moment';
import { CustomTabs } from '@choerodon/components';
import { LabelLayout } from 'choerodon-ui/pro/lib/form/enum';
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
      style={{ width: 240 }}
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
      style={{ marginLeft: 24, width: 230 }}
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
    <div style={{ marginTop: 20, display: 'flex', alignItems: 'center' }}>
      <DatePicker
        range
        style={{ width: 497 }}
        placeholder={['开始时间', '结束时间']}
        label=" "
        value={range}
        onChange={onRangeChange}
        clearButton={false}
        labelLayout={'float' as LabelLayout}
        max={moment()}
      />
      <div style={{ marginLeft: 20 }}>
        <CustomTabs
          // @ts-ignore
          selectedTabValue={days}
          onChange={(a, b, c) => setDays(c)}
          data={[{
            name: '今天',
            value: 0,
          },
          {
            name: '近7天',
            value: 7,
          },
          {
            name: '近30天',
            value: 30,
          }]}
        />
      </div>
    </div>
  </div>
);
export default ServiceCodeQualityVarySearch;
