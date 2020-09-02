// import React, {
//   useEffect, useRef, useState, useCallback,
// } from 'react';
// import {
//   Form, TextField, DataSet, Modal,
// } from 'choerodon-ui/pro';
// import { observer } from 'mobx-react-lite';
// import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
// // @ts-ignore
// import UploadButton from '@/components/CommonComponent/UploadButton';
// import validateFile from '@/utils/File';
// import CustomField, { IField } from '@/components/custom-field';

// interface Props {
//   onCreate: (demand: Demand) => void,
//   organizationId: number,
//   modal: Modal,
//   project?: IAMProject,
// }
// const defaultDataSet = new DataSet({
//   autoCreate: true,
//   fields: [{
//     name: 'projectId',
//     type: 'number' as FieldType,
//     textField: 'name',
//     valueField: 'id',
//     label: '所属需求池',
//     required: true,
//   }],
// });
// const presets = new Map([
//   ['summary', {
//     name: 'summary',
//     type: 'string' as FieldType,
//   }],
//   ['urgent', {
//     name: 'priorityId',
//     type: 'string' as FieldType,
//     label: '紧急程度',
//     textField: 'name',
//     valueField: 'id',
//   }],
//   ['backlogClassification', {
//     name: 'classificationId',
//     type: 'string' as FieldType,
//     label: '需求分类',
//     textField: 'name',
//     valueField: 'id',
//     // @ts-ignore
//     dynamicProps: {
//       // @ts-ignore
//       disabled: ({ record }) => {
//         if (record.get('projectId')) {
//           return false;
//         }
//         return true;
//       },
//     },
//   }],
//   ['backlogType', {
//     name: 'typeId',
//     type: 'string' as FieldType,
//     label: '需求类型',
//     textField: 'name',
//     valueField: 'id',
//     dynamicProps: {
//       // @ts-ignore
//       disabled: ({ record }) => {
//         if (record.get('projectId')) {
//           return false;
//         }
//         return true;
//       },
//     },
//   }],
//   ['description', {
//     name: 'description',
//     type: 'array' as FieldType,
//     label: '描述',
//   }],
//   ['belongToBacklog', {
//     name: 'projectId',
//     type: 'number' as FieldType,
//     textField: 'name',
//     valueField: 'id',
//     label: '所属需求池',
//     required: true,
//   }],
//   ['progressFeedback', {
//     name: 'feedBack',
//     type: 'boolean' as FieldType,
//     label: '需求反馈进度',
//   }],
//   ['estimatedStartTime', {
//     name: 'estimatedStartTime',
//     type: 'string' as FieldType,
//     label: '预计开始时间',
//     max: 'estimatedEndTime',
//   }],
//   ['estimatedEndTime', {
//     name: 'estimatedEndTime',
//     type: 'string' as FieldType,
//     label: '预计结束时间',
//     min: 'estimatedStartTime',
//   }],
//   ['feedbackFrequency', {
//     name: 'feedbackFrequency',
//     type: 'number' as FieldType,
//     label: '',
//     min: 1,
//     defaultValue: 7,
//   }],
// ]);
// // @ts-ignore
// const CreateIssue: React.FC<Props> = observer(({
//   onCreate, organizationId, modal, project,
// }) => {
//   const [fileList, setFileList] = useState<FileList>();
//   const [fields, setFields] = useState<IField[]>([{
//     fieldCode: 'belongToBacklog',
//   } as IField]);
//   const firstRef = useRef<Form>(null);
//   const dataSetRef = useRef(defaultDataSet);
//   const dataSet = dataSetRef.current;

//   const renderField = (field: IField) => {
//     switch (field.fieldCode) {
//       case 'summary': {
//         return <TextField colSpan={2} name="summary" autoFocus />;
//       }
//       // case 'description': {
//       //   return <Editor name="description" colSpan={2} placeholder="需求描述" />;
//       // }
//       case 'estimatedStartTime': {
//         return (
//           <CustomField
//             field={field}
//             name={field.fieldCode}
//           />
//         );
//       }
//       case 'estimatedEndTime': {
//         return (
//           <CustomField
//             field={field}
//             name={field.fieldCode}
//           />
//         );
//       }
//       default: {
//         return (
//           <CustomField
//             field={field}
//             name={field.fieldId}
//           />
//         );
//       }
//     }
//   };
//   return (
//     <Form dataSet={dataSet} style={{ width: 688 }} columns={2} ref={firstRef}>
//       {fields.map(renderField)}
//       <UploadButton
//         // @ts-ignore
//         fileList={fileList}
//         colSpan={2}
//         // @ts-ignore
//         onChange={({ fileList: files }) => {
//           if (validateFile(files)) {
//             setFileList(files);
//           }
//         }}
//       />
//     </Form>
//   );
// });

// const openModal = () => {
//   Modal.open({
//     drawer: true,
//     style: {
//       width: 1088,
//     },
//     key: 'outsideDemand',
//     title: '创建问题',
//     okText: '提交',
//     // @ts-ignore
//     children: <CreateIssue onCreate={onCreate} />,
//     footer: null,
//   });
// };
// export default openModal;
