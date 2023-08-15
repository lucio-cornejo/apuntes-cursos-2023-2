# Building process

Set up a `courses.json` file of the form:

```js
{
  "course-1-alias": "Course_1 name",
  "course-2-alias": "Course_2 name",
  ...
}
```

## Create folders for courses
  
Execute `npm run new-courses` .

## Create new course class

Execute `npm run new-class -- course_alias class_number` .

## Move course class notes and subset its pdf

Execute `npm run save-class -- first_n_pages_to_keep` .
