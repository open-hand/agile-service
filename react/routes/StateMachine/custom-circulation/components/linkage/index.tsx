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
import { issueLinkTypeApi, statusTransformApi } from '@/api';
import Loading from '@/components/Loading';
import DataSetField from 'choerodon-ui/pro/lib/data-set/Field';
import useFields from '@/routes/Issue/components/BatchModal/useFields';
import { getIsOrganization } from '@/utils/common';
import { IIssueType } from '@/common/types';
import HostPick from '@/components/host-pick';
import styles from './index.less';

interface IParentIssueStatusSetting {
  id: string
  issueTypeId: string
  parentIssueStatusSetting: string
  parentIssueTypeId: string
  projectId: number
  statusId: string
}

interface ILinkIssueStatusSetting {
  linkTypeId: string
  linkIssueTypeId: string
  linkIssueStatusId: string
}

interface ILinkType {
  linkTypeId: string
  linkName: string
}

const { Option } = Select;
const Linkage = ({
  // @ts-ignore
  modal, record, selectedType, customCirculationDataSet, linkageType,
}) => {
  const isOrganization = getIsOrganization();
  const { data: issueTypes } = useIssueTypes({ typeCode: ['story', 'task', 'bug'] });
  const [fields, Field] = useFields();
  const [linkFields, LinkField] = useFields();
  const [loading, setLoading] = useState(false);
  const [activeKey, setActiveKey] = useState<'subIssue' | 'linkIssue'>(linkageType[0]);
  const [linkTypes, setLinkTypes] = useState<ILinkType[]>([]);

  useEffect(() => {
    if (linkageType.includes('linkIssue')) {
      issueLinkTypeApi.getAll().then((res: { content: ILinkType[]}) => {
        setLinkTypes(res.content || []);
      });
    }
  }, [linkageType]);
  const linkageDataSet = useMemo(() => new DataSet({
    autoCreate: true,
  }), []);
  const linkIssueLinkageDataSet = useMemo(() => new DataSet({
    autoCreate: true,
  }), []);
  const getFieldValue = useCallback((ds, name) => {
    const { current } = ds;
    if (current) {
      return current.get(name);
    }
    return '';
  }, []);
  const getField = useCallback((ds, name) => ds.current?.getField(name), []);

  const addField = useCallback((ds, name, props) => {
    const field = new DataSetField({ ...props, name }, ds, ds.current);
    ds?.current?.fields.set(name, field);
  }, []);

  const addFieldRule = useCallback((key) => {
    addField(linkageDataSet, `${key}-type`, {
      required: true,
    });
    addField(linkageDataSet, `${key}-status`, {
      required: true,
    });
  }, [addField, linkageDataSet]);

  const linkIssueAddFieldRule = useCallback((key) => {
    addField(linkIssueLinkageDataSet, `${key}-linkTypeId`, {
      required: true,
    });
    addField(linkIssueLinkageDataSet, `${key}-linkIssueTypeId`, {
      required: true,
    });
    addField(linkIssueLinkageDataSet, `${key}-linkIssueStatusId`, {
      required: true,
    });
  }, [addField, linkIssueLinkageDataSet]);

  const setFieldValue = useCallback((ds: DataSet, name: string, value: any) => {
    const { current } = ds;
    if (current) {
      current.set(name, value);
    }
  }, []);
  const removeField = useCallback((name) => {
    linkageDataSet.fields.delete(name);
    linkageDataSet.current?.fields.delete(name);
  }, [linkageDataSet]);
  useEffect(() => {
    const getLinkages = async () => {
      setLoading(true);
      try {
        if (linkageType.includes('subIssue')) {
          const res: IParentIssueStatusSetting[] = await statusTransformApi.getLinkage(selectedType, record.get('id'));
          const initFields = Field.init(new Array(Math.max(res.length, 1)).fill({}));
          initFields.forEach((item: { key: number }, i: number) => {
            addFieldRule(item.key);
            setFieldValue(linkageDataSet, `${item.key}-type`, res[i]?.parentIssueTypeId);
            setFieldValue(linkageDataSet, `${item.key}-status`, res[i]?.parentIssueStatusSetting);
          });
        }
        if (linkageType.includes('linkIssue')) {
          const res: ILinkIssueStatusSetting[] = await statusTransformApi.getLinkIssueLinkage(selectedType, record.get('id'));
          const initFields = LinkField.init(new Array(Math.max(res.length, 1)).fill({}));
          initFields.forEach((item: { key: number }, i: number) => {
            linkIssueAddFieldRule(item.key);
            setFieldValue(linkIssueLinkageDataSet, `${item.key}-linkTypeId`, res[i]?.linkTypeId);
            setFieldValue(linkIssueLinkageDataSet, `${item.key}-linkIssueTypeId`, res[i]?.linkIssueTypeId);
            setFieldValue(linkIssueLinkageDataSet, `${item.key}-linkIssueStatusId`, res[i]?.linkIssueStatusId);
          });
        }
        setLoading(false);
      } catch (err) {
        console.log(err);
        setLoading(false);
      }
    };
    getLinkages();
  }, [Field, LinkField, addFieldRule, linkIssueAddFieldRule, linkIssueLinkageDataSet, linkageDataSet, linkageType, record, selectedType, setFieldValue]);
  useEffect(() => {
    const handleOk = async () => {
      const linkageValidate = await linkageDataSet.validate();
      const linkIssueLinkageValidate = await linkIssueLinkageDataSet.validate();
      if (linkageValidate && linkIssueLinkageValidate) {
        if (linkageType.includes('subIssue')) {
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
        }
        if (linkageType.includes('linkIssue')) {
          const data: any = linkIssueLinkageDataSet.current?.toData();
          const updateData: any[] = [];
          linkFields.forEach((f: any) => {
            const { key } = f;
            updateData.push({
              linkTypeId: data[`${key}-linkTypeId`],
              linkIssueTypeId: data[`${key}-linkIssueTypeId`],
              linkIssueStatusId: data[`${key}-linkIssueStatusId`],
            });
          });
          // @ts-ignore
          await statusTransformApi.updateLinkIssueLinkage(selectedType, record.get('id'), updateData);
        }
        customCirculationDataSet.query(customCirculationDataSet.currentPage);
        return true;
      }
      if (!linkageValidate) {
        if (linkageType.length === 2 && activeKey === 'linkIssue') {
          setActiveKey('subIssue');
        }
        return false;
      }
      if (!linkIssueLinkageValidate) {
        if (linkageType.length === 2 && activeKey === 'subIssue') {
          setActiveKey('linkIssue');
        }
        return false;
      }
      return false;
    };
    if (modal) {
      modal.handleOk(handleOk);
    }
  }, [activeKey, customCirculationDataSet, fields, isOrganization, linkFields, linkIssueLinkageDataSet, linkageDataSet, linkageType, modal, record, selectedType]);
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

  const selectedLinkTypes = (() => {
    const data: any = linkIssueLinkageDataSet.toData()[0];
    return Object.keys(data).reduce((result: string[], key) => {
      const [k, code] = key.split('-');
      if (code === 'linkTypeId') {
        result.push(data[key]);
      }
      return result;
    }, []);
  })();

  const handleActiveKeyChange = useCallback((key) => {
    setActiveKey(key);
  }, []);
  return (
    <div className={styles.linkage}>
      <Loading loading={loading} />
      {
        linkageType.length === 1 && (
          <div className={styles.tip}>{linkageType[0] === 'subIssue' ? '当工作项流转到此状态后，关联的父任务状态设置。' : `当问题流转到${record.get('name')}后，关联的问题满足下列设置，将自动流转到指定状态。`}</div>
        )
      }
      {
        linkageType.length === 2 && (
          <div className={styles.typePick}>
            <HostPick
              defaultActiveKey="subIssue"
              onChange={handleActiveKeyChange}
              hostTabKeys={[{
                key: 'subIssue',
                text: '父子级联动',
              }, {
                key: 'linkIssue',
                text: '关联问题联动',
              }]}
            />
            <div className={styles.linkageTip}>{activeKey === 'subIssue' ? '当工作项流转到此状态后，关联的父任务状态设置。' : `当问题流转到${record.get('name')}后，关联的问题满足下列设置，将自动流转到指定状态。`}</div>
          </div>
        )
      }
      <div>
        {
          linkageType.includes('subIssue') && activeKey === 'subIssue' && (
            <Form dataSet={linkageDataSet}>
              {fields.map((f: any) => {
                const { key, id } = f;
                const typeName = `${key}-type`;
                const statusName = `${key}-status`;
                const issueTypeId = getFieldValue(linkageDataSet, typeName);
                return (
                  <Row key={key} gutter={20} type="flex" align="middle">
                    <Col span={11}>
                      <Select
                        label="父任务类型"
                        name={typeName}
                        onChange={() => {
                    getField(linkageDataSet, statusName)?.reset();
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
          )
        }
        {
          linkageType.includes('linkIssue') && activeKey === 'linkIssue' && (
            <Form dataSet={linkIssueLinkageDataSet}>
              {linkFields.map((f: any) => {
                const { key, id } = f;
                const linkTypeName = `${key}-linkTypeId`;
                const typeName = `${key}-linkIssueTypeId`;
                const statusName = `${key}-linkIssueStatusId`;
                const linkTypeId = getFieldValue(linkIssueLinkageDataSet, linkTypeName);
                const issueTypeId = getFieldValue(linkIssueLinkageDataSet, typeName);
                return (
                  <Row key={key} gutter={20} type="flex" align="middle">
                    <Col span={8}>
                      <Select
                        label="关联类型"
                        name={linkTypeName}
                        onChange={() => {
                          getField(linkIssueLinkageDataSet, typeName)?.reset();
                          getField(linkIssueLinkageDataSet, statusName)?.reset();
                          linkIssueLinkageDataSet.current?.init(typeName, undefined);
                          linkIssueLinkageDataSet.current?.init(statusName, undefined);
                        }}
                      >
                        {linkTypes?.filter((type: ILinkType) => (linkTypeId && type.linkTypeId === linkTypeId) || !selectedLinkTypes.includes(type.linkTypeId)).map((type: ILinkType) => (
                          <Option value={type.linkTypeId}>
                            {type.linkName}
                          </Option>
                        ))}
                      </Select>
                    </Col>
                    <Col span={7}>
                      <Select
                        label="问题类型"
                        name={typeName}
                        onChange={() => {
                          getField(linkIssueLinkageDataSet, statusName)?.reset();
                          linkIssueLinkageDataSet.current?.init(statusName, undefined);
                        }}
                      >
                        {issueTypes?.filter((type: IIssueType) => ['story', 'task', 'bug'].includes(type.typeCode)).map((type: IIssueType) => (
                          <Option value={type.id}>
                            {type.name}
                          </Option>
                        ))}
                      </Select>
                    </Col>
                    <Col span={7} key={id}>
                      <SelectStatus
                        label="指定状态"
                        key={`${key}-${selectedType}-${issueTypeId}`}
                        name={statusName}
                        request={selectedType && issueTypeId ? () => statusTransformApi.getLinkageStatus(selectedType, issueTypeId) : () => {}}
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
                          LinkField.remove(key);
                          removeField(linkTypeName);
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
                    const newKey = LinkField.add();
                    linkIssueAddFieldRule(newKey);
                  }}
                >
                  添加联动
                </Button>
              </div>
            </Form>
          )
        }
      </div>

    </div>
  );
};

export default observer(Linkage);
