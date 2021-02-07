const fields2Map = (fields) => new Map(fields.map((field) => ([field.fieldCode, field])));

const getDefaultValueMap = (fields) => {
  const defaultScope = new Map([
    ['component', 'componentIssueRel'],
    ['label', 'issueLabel'],
    ['fixVersion', 'fixVersionIssueRel'],
    ['influenceVersion', 'influenceVersion'],
    ['sprint', 'sprintId'],
    ['epic', 'epicId'],
  ]);
  const fieldsObj = fields.reduce((result, field) => {
    const name = defaultScope.get(field.fieldCode);
    if (name && field.defaultValue) {
      Object.assign(result, {
        [name]: field.defaultValue,
      });
    }
    return result;
  }, {});
  return fieldsObj;
};
export { getDefaultValueMap, fields2Map };
