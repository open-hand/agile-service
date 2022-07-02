import React, {useCallback, useImperativeHandle, useState} from 'react';
import {Choerodon} from '@choerodon/boot';
import {epicApi, issueApi} from '@/api';
import Card from './Card';
import StoryMapStore from '../../../../../stores/project/StoryMap/StoryMapStore';
import clickOutSide from '../../../../../components/CommonComponent/ClickOutSide';
import {TextArea} from 'choerodon-ui/pro';
import {ResizeType} from 'choerodon-ui/pro/lib/text-area/enum'
import {MAX_LENGTH_EPIC_NAME} from "@/constants/MAX_LENGTH";

interface Props {
  epic: {
    epicName: string
    issueId: string
    objectVersionNumber: number
  }
  setEditing: (editing: boolean) => void
  handleClickOutside: () => void
  editEpicRef: React.MutableRefObject<{
    handleEditEpic: (value: string) => Promise<void>
    value: string
  }>
}

const EditEpic: React.FC<Props> = ({
  epic, setEditing, handleClickOutside, editEpicRef,
}) => {
  const [value, setValue] = useState(epic.epicName);
  let canEdit: boolean = true;
  const handleChange = useCallback((e) => {
    setValue(e.target.value);
  }, []);

  const handleEditEpic = useCallback(async (newValue) => {
    console.log('pressEnter');

    if (!canEdit) {
      return;
    }
    canEdit = false;
    if (newValue !== '' && newValue.trim()) {
      epicApi.checkName(newValue, epic.issueId).then((checkRes: boolean) => {
        if (checkRes) {
          Choerodon.prompt('史诗名称重复');
        } else {
          const req = {
            objectVersionNumber: epic.objectVersionNumber,
            issueId: epic.issueId,
            epicName: newValue,
          };
          issueApi.update(req).then((res) => {
            setEditing(false);
            StoryMapStore.afterEditEpicName(res);
          }).catch((e) => {
            setEditing(false);
            setValue(epic.epicName);
            Choerodon.prompt('修改史诗失败');
          });
        }
      }).finally(() => {
        canEdit = true;
      });
    } else {
      canEdit = true;
      setEditing(false);
      setValue(epic.epicName);
    }
  }, []);

  useImperativeHandle(editEpicRef, () => ({
    handleEditEpic,
    value,
  }));

  return (
    <div>
      <Card style={{
        boxShadow: '0 0 4px -2px rgba(0,0,0,0.50), 0 2px 4px 0 rgba(0,0,0,0.13)',
        borderRadius: 2,
        height: 64,
        margin: '4px 4px 4px 9px',
        padding: 7,
        display: 'flex',
        justifyContent: 'center',
      }}
      >
        <TextArea autoFocus onEnterDown={() => handleEditEpic(value)} placeholder="修改史诗名称" rows={1}
                  resize={ResizeType.vertical}
                  maxLength={MAX_LENGTH_EPIC_NAME} showLengthInfo={true} value={value} onInput={handleChange} />
      </Card>
    </div>
  );
};

export default clickOutSide(EditEpic);
