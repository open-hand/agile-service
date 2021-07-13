import React from 'react';
import { fieldApi } from '@/api';
import { useQuery } from 'react-query';
import { Checkbox, Row, Col } from 'choerodon-ui';
import useProjectKey from '@/hooks/data/useProjectKey';

interface BasicProps {
  projectId?: string
  organizationId?: string
  outside?: boolean
  label?: string
}
// 参数互斥，要么传fieldId，要么传fieldOptions
type SelectCheckBoxOldProps = BasicProps & ({
  fieldId: string
  fieldOptions?: never
  onlyEnabled?: boolean
} | {
  fieldId?: never
  // 把所有options传进来，前端假分页
  fieldOptions: {
    id: string
    value: string
    enabled: boolean
  }[]
  onlyEnabled?: boolean
})

const SelectCheckBoxOld: React.FC<SelectCheckBoxOldProps> = ({
  fieldId, fieldOptions, projectId, organizationId, outside = false, onlyEnabled = true, ...otherProps
}) => {
  const key = useProjectKey({ key: ['fieldOptions', fieldId] });
  const { data } = useQuery(key, () => fieldApi
    .outside(outside)
    .org(organizationId)
    .project(projectId)
    .getFieldOptions(fieldId!, '', 1, 0, [], onlyEnabled));
  const options = data ? data.list : [];
  return (
    <Checkbox.Group
      {...otherProps}
    >
      {options.length > 0
        ? (
          <Row>
            {options.filter((option: any) => option.enabled).map((item: any) => (
              <Col
                span={24}
                key={item.id}
                style={{ width: 'auto' }}
              >
                <Checkbox
                  value={item.id}
                  key={item.id}
                  className="checkboxStyle"
                >
                  {item.value}
                </Checkbox>
              </Col>
            ))}
            {' '}

          </Row>
        ) : <span style={{ color: '#D50000' }}>暂无选项，请联系管理员</span>}
    </Checkbox.Group>
  );
};
export default SelectCheckBoxOld;
