import React from 'react';
import { Select } from 'choerodon-ui/pro';
import { LabelLayout } from 'choerodon-ui/pro/lib/form/enum';
import SelectService from './SelectService';

const { Option } = Select;
export type ServiceCodeQualityType = 'issue' | 'coverage' | 'duplicate'
export interface ServiceCodeQualitySearchProps {
  projectId: string
  serviceId: string
  setServiceId: (serviceId: string) => void
  days: number
  setDays: (days: number) => void
  type: ServiceCodeQualityType
  setType: (type: ServiceCodeQualityType) => void
  onEmpty: () => void
}
const ServiceCodeQualityVarySearch: React.FC<ServiceCodeQualitySearchProps> = ({
  days,
  setDays,
  serviceId,
  setServiceId,
  type,
  setType,
  onEmpty,
}) => (
  <div>
    <SelectService
      labelLayout={'float' as LabelLayout}
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
    />
    <Select value={type} onChange={setType} labelLayout={'float' as LabelLayout}>
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
    <Select value={days} onChange={setDays} labelLayout={'float' as LabelLayout}>
      <Option value={7}>
        近7天
      </Option>
      <Option value={15}>
        近15天
      </Option>
      <Option value={30}>
        近30天
      </Option>
    </Select>
  </div>
);
export default ServiceCodeQualityVarySearch;
