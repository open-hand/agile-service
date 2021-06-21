import React, {
  useMemo, useEffect, useCallback, useState,
} from 'react';
import {
  Form, DataSet, Button, Select, Row, Col,
} from 'choerodon-ui/pro';
import { Icon } from 'choerodon-ui';
import { observer } from 'mobx-react-lite';
import useIssueTypes from '@/hooks/data/useIssueTypes';
import SelectStatus from '@/components/select/select-status';
import { statusTransformApi } from '@/api';
import Loading from '@/components/Loading';
import DataSetField from 'choerodon-ui/pro/lib/data-set/Field';
import useFields from '@/routes/Issue/components/BatchModal/useFields';
import { getIsOrganization } from '@/utils/common';
import { IIssueType } from '@/common/types';
import styles from './index.less';

interface IParentIssueStatusSetting {
  id: string
  issueTypeId: string
  parentIssueStatusSetting: string
  parentIssueTypeId: string
  projectId: number
  statusId: string
}
const { Option } = Select;
const Linkage = ({
  // @ts-ignore
  modal, record, selectedType, customCirculationDataSet,
}) => {
  const isOrganization = getIsOrganization();
  const { data: issueTypes } = useIssueTypes({ typeCode: ['story', 'task', 'bug'] });
  const [fields, Field] = useFields();
  const [loading, setLoading] = useState(false);
  const linkageDataSet = useMemo(() => new DataSet({
    autoCreate: true,
  }), []);
  const getFieldValue = useCallback((name) => {
    const { current } = linkageDataSet;
    if (current) {
      return current.get(name);
    }
    return '';
  }, [linkageDataSet]);
  const getField = useCallback((name) => linkageDataSet.current?.getField(name), [linkageDataSet]);

  const addField = useCallback((name, props) => {
    const field = new DataSetField({ ...props, name }, linkageDataSet, linkageDataSet.current);
    linkageDataSet?.current?.fields.set(name, field);
  }, [linkageDataSet]);

  const addFieldRule = useCallback((key) => {
    addField(`${key}-type`, {
      required: true,
    });
    addField(`${key}-status`, {
      required: true,
    });
  }, [addField]);

  const setFieldValue = useCallback((name: string, value: any) => {
    const { current } = linkageDataSet;
    if (current) {
      current.set(name, value);
    }
  }, [linkageDataSet]);
  const removeField = useCallback((name) => {
    linkageDataSet.fields.delete(name);
    linkageDataSet.current?.fields.delete(name);
  }, [linkageDataSet]);
  useEffect(() => {
    setLoading(true);
    statusTransformApi.getLinkage(selectedType, record.get('id')).then((res: IParentIssueStatusSetting[]) => {
      const initFields = Field.init(new Array(Math.max(res.length, 1)).fill({}));
      initFields.forEach((item: { key: number }, i: number) => {
        addFieldRule(item.key);
        setFieldValue(`${item.key}-type`, res[i].parentIssueTypeId);
        setFieldValue(`${item.key}-status`, res[i].parentIssueStatusSetting);
      });
      setLoading(false);
    }).catch(() => {
      setLoading(false);
    });
  }, [Field, addFieldRule, linkageDataSet, record, selectedType, setFieldValue]);
  useEffect(() => {
    const handleOk = async () => {
      if (await linkageDataSet.validate()) {
        const data: any = linkageDataSet.current?.toData();
        const updateData: any[] = [];
        fields.forEach((f: any) => {
          const { key } = f;
          updateData.push({
            parentIssueTypeId: data[`${key}-type`],
            parentIssueStatusSetting: data[`${key}-status`],
          });
        });
        // @ts-ignore
        await statusTransformApi[isOrganization ? 'orgUpdateLinkage' : 'updateLinkage'](selectedType, record.get('id'), record.get('objectVersionNumber'), updateData);
        customCirculationDataSet.query(customCirculationDataSet.currentPage);
        return true;
      }
      return false;
    };
    if (modal) {
      modal.handleOk(handleOk);
    }
  }, [customCirculationDataSet, fields, isOrganization, linkageDataSet, modal, record, selectedType]);
  const selectedIssueTypes = (() => {
    const data: any = linkageDataSet.toData()[0];
    return Object.keys(data).reduce((result: string[], key) => {
      const [k, code] = key.split('-');
      if (code === 'type') {
        result.push(data[key]);
      }
      return result;
    }, []);
  })();
  return (
    <div className={styles.linkage}>
      <Loading loading={loading} />
      <div className={styles.tip}>当工作项流转到此状态后，关联的父任务状态设置。</div>
      <Form dataSet={linkageDataSet}>
        {fields.map((f: any) => {
          const { key, id } = f;
          const typeName = `${key}-type`;
          const statusName = `${key}-status`;
          const issueTypeId = getFieldValue(typeName);
          return (
            <Row key={key} gutter={20} type="flex" align="middle">
              <Col span={11}>
                <Select
                  label="父任务类型"
                  name={typeName}
                  onChange={() => {
                    getField(statusName)?.reset();
                    linkageDataSet.current?.init(statusName, undefined);
                  }}
                >
                  {issueTypes?.filter((type: IIssueType) => (issueTypeId && type.id === issueTypeId) || !selectedIssueTypes.includes(type.id)).map((type: IIssueType) => (
                    <Option value={type.id}>
                      {type.name}
                    </Option>
                  ))}
                </Select>
              </Col>
              <Col span={11} key={id}>
                <SelectStatus
                  label="指定状态"
                  key={`${key}-${issueTypeId}`}
                  name={statusName}
                  issueTypeId={issueTypeId}
                  isOrganization={isOrganization}
                />
              </Col>
              <Col span={2}>
                <Icon
                  type="delete_sweep-o"
                  style={{
                    color: 'var(--primary-color)',
                    cursor: 'pointer',
                  }}
                  onClick={() => {
                    Field.remove(key);
                    removeField(typeName);
                    removeField(statusName);
                  }}
                />
              </Col>
            </Row>
          );
        })}
        <div>
          <Button
            style={{ marginTop: -4 }}
            icon="playlist_add"
            onClick={() => {
              const newKey = Field.add();
              addFieldRule(newKey);
            }}
          >
            添加联动
          </Button>
        </div>
      </Form>
    </div>
  );
};

export default observer(Linkage);
