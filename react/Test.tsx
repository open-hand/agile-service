import React from 'react';
import { TextField, TextArea } from 'choerodon-ui/pro';
import TextEditToggle from '@/components/TextEditTogglePro';
import SelectUser from '@/components/select/select-user';

const Test = () => (
  <div>
    <TextEditToggle
      onSubmit={async (data) => {
        await new Promise((resolve) => {
          setTimeout(() => {
            resolve();
          }, 2000);
        });
        // console.log(data);
      }}
      initValue="第一个"
      renderText={text => text}
    >
      <TextField style={{
        width: 100,
      }}
      />
    </TextEditToggle>
    <TextEditToggle
      onSubmit={async (data) => {
        await new Promise((resolve) => {
          setTimeout(() => {
            resolve();
          }, 2000);
        });
        // console.log(data);
      }}
      initValue="第二个"
      renderText={text => text}
    >
      <TextArea
        style={{
          width: 100,
        }}
        tabIndex={0}
      />
    </TextEditToggle>
    <TextEditToggle
      onSubmit={(data) => {
        // console.log(data);
      }}
      initValue={undefined}
      renderText={text => <span>user</span>}
    >
      <SelectUser />
    </TextEditToggle>
      
  </div>
);


export default Test;
