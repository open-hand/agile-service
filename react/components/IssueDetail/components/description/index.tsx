import React, { useState, useEffect } from 'react';
import { Tooltip, Button } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import WYSIWYGEditor from '@/components/CKEditor';
import WYSIWYGViewer from '@/components/WYSIWYGViewer';
import FullEditor from '@/components/FullEditor';
import { text2Delta } from '@/utils/richText';
import Section from '../section';
import { useDetailContext } from '../../context';

const Description: React.FC = () => {
  const [edit, setEdit] = useState(false);
  const [fullEdit, setFullEdit] = useState(false);

  const { store, disabledDetailEdit } = useDetailContext();
  const { description } = store.issue;
  const [delta, setDelta] = useState<string>(description);

  useEffect(() => {
    setDelta(text2Delta(description));
    setEdit(false);
  }, [description]);

  const updateIssueDes = async (value: string = delta) => {
    if (value) {
      await store.update('description', value);
      setEdit(false);
      setFullEdit(false);
    }
  };

  return (
    <Section
      title="描述"
      border
      buttons={!disabledDetailEdit ? (
        <>
          <Tooltip title="全屏编辑">
            <Button
              style={{ padding: '0 6px' }}
              color={'blue' as ButtonColor}
              icon="zoom_out_map"
              onClick={() => {
                setFullEdit(true);
              }}
            />
          </Tooltip>
          <Tooltip placement="topRight" autoAdjustOverflow={false} title="编辑">
            <Button
              style={{ padding: '0 6px' }}
              color={'blue' as ButtonColor}
              icon="mode_edit"
              onClick={() => {
                setEdit(true);
              }}
            />
          </Tooltip>
        </>
      ) : ''}
    >
      <div>
        {edit ? (
          <WYSIWYGEditor
            autoFocus
            footer
            value={delta}
            style={{
              minHeight: 260, width: '100%',
            }}
            onChange={(value) => {
              setDelta(value);
            }}
            onCancel={() => {
              setEdit(false);
              setDelta(text2Delta(description));
            }}
            onOk={updateIssueDes}
          />
        ) : <WYSIWYGViewer data={description} />}
        {
          fullEdit ? (
            <FullEditor
              autoFocus
              initValue={delta}
              visible={fullEdit}
              onCancel={() => setFullEdit(false)}
              onOk={updateIssueDes}
            />
          ) : null
        }
      </div>
    </Section>
  );
};

export default observer(Description);
